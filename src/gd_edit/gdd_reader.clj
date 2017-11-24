(ns gd-edit.gdd-reader
  (:require [gd-edit.structure :as s]
            [gd-edit.utils :as utils]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [gd-edit.utils :as u]
            [gd-edit.globals :as globals]
            [gd-edit.gdc-reader :as gdc]
            [clojure.inspector])
  (:import  [java.nio ByteBuffer ByteOrder]
            [java.io FileOutputStream]))

(declare read-block read-block-start read-and-verify-block-end)

(def FilePreamble
  (s/ordered-map
   :magic   :int32
   :version :int32))


(def UID
  (s/string :bytes :length 16))


;; Tokens block
(def Block10
  ^{:known-versions #{2}}
  (s/ordered-map
   :version :int32
   :tokens (s/variable-count (s/string :ascii))))


(defn objectives-count
  [bb data context]

  (let [count (-> (:block-stack @context)
                  (first)
                  (:length)
                  (- 5)
                  (/ 4))]
    count))


(def TaskInnerBlock
  (s/ordered-map
   :state :int32
   :in-progress :byte
   :objectives (s/variable-count :int32
                                 :read-length-fn objectives-count
                                 :skip-write-length true)))


(defn read-task-block
  [bb context]

  (let [data {:id1 (gdc/read-int! bb context)
              :id2 (s/read-struct UID bb context)}

        tasks-rest (read-block bb context {0 TaskInnerBlock})]

    (merge data tasks-rest)))


(def Task
  (gdc/merge-meta
   (s/ordered-map
    :id1 :int32
    :id2 UID

    ;; :state :int32
    ;; :in-progress :byte
    ;; :objectives (s/variable-count :int32
    ;;                               :read-length-fn objectives-count
    ;;                               :skip-write-length true)
    )
   {:struct/read read-task-block}))


(def TaskListBlock
  (s/ordered-map
   :tasks (s/variable-count Task)))


(defn read-quest-block
  [bb context]

  (let [data {:id1 (gdc/read-int! bb context)
              :id2 (s/read-struct UID bb context)}
        tasks (read-block bb context {0 TaskListBlock})]

    (assoc data :tasks (:tasks tasks))))


(def Quest
  (gdc/merge-meta
   (s/ordered-map
    :id1 :int32
    :id2 UID
    :tasks (s/variable-count Task))

   {:struct/read read-quest-block}))


;; Quests block
(def Block11
  ^{:known-versions #{3}}
  (s/ordered-map
   :version :int32
   :quests (s/variable-count Quest)))


(defn- validate-preamble
  [preamble]

  (if (or (not= (:magic preamble) 0x58545351)
          (not= (:version preamble) 0))
    (throw (Throwable. "I don't understand this gdd format!"))))


(defn read-block-start
  [bb context]

  (let [id (gdc/read-int! bb context)
        length (gdc/decrypt-int (.getInt bb) (:enc-state @context))
        expected-end-position (+ (.position bb) length)]
    {:id id
     :length length
     :expected-end-position expected-end-position}))


(defn read-and-verify-block-end
  [bb context id expected-end-position]

  ;; Verify we've reached the expected position
  (assert (= expected-end-position (.position bb))
          (utils/fmt "[block #{id}] Expected to be at stream position: #{expected-end-position}, but current at #{(.position bb)}. Position is off by: #{(- expected-end-position (.position bb) )}"))

  ;; Verify we have the correct enc-state at this point
  (let [checksum (Integer/toUnsignedLong (.getInt bb))]
    (assert (= checksum (:enc-state @context)))))


(defn read-block
  ([^ByteBuffer bb context]
   (read-block bb context nil))

  ([^ByteBuffer bb context block-spec-overrides]

   (let [{:keys [id length expected-end-position] :as block-header} (read-block-start bb context)
         ;; _ (println "Reading block:" id)
         ;; _ (println "length:" length)

         ;; Push data about the current block into a stack
         _ (swap! context update-in [:block-stack] conj block-header)

         ;; Figure out how we can read the block
         block-spec-or-read-fn (or (get block-spec-overrides id)
                                   (gdc/get-block-read-fn id)
                                   (gdc/get-block-spec id))

         ;; If neither a block spec or a custom read function can be found...
         ;; We don't know how to read this block
         _ (if (nil? block-spec-or-read-fn)
             (throw (Throwable. (str"Don't know how to read block " id))))

         ;; Try to read the block
         ;; If a custom read function was provided, use that
         ;; Otherwise, try to read using a block spec
         block-data (cond
                      (fn? block-spec-or-read-fn)
                      (block-spec-or-read-fn bb context)
                      :else
                      (s/read-struct block-spec-or-read-fn bb context {}))

         ;; Verify that this is a block version we understand
         _ (if-let [known-versions (:known-versions (meta block-spec-or-read-fn))]
             (assert (contains? known-versions (:version block-data))))
         ]

     ;; Verify we have the correct enc-state at this point
     (read-and-verify-block-end bb context id expected-end-position)

     ;; Pop from the block stack
     (swap! context update-in [:block-stack] pop)

     (assoc block-data :meta-block-id id))))


(defn load-quest-file
  [filepath]

  (let [bb ^ByteBuffer (utils/file-contents filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        seed (bit-xor (Integer/toUnsignedLong (.getInt bb)) 0x55555555)
        enc-table (gdc/generate-encryption-table seed)
        enc-context (gdc/make-enc-context seed enc-table)

        preamble (s/read-struct FilePreamble bb enc-context)
        _ (validate-preamble preamble)

        id (s/read-struct UID bb enc-context)

        ;; ;; Keep reading more blocks until we've reached the end of the file
        block-list (->> (loop [block-list (transient [])]
                          (if (= (.remaining bb) 0)
                            (persistent! block-list)

                            (recur (conj! block-list (read-block bb enc-context)))))

                        ;; Append the header block when we're done reading
                        (into []))

        ;; ;; Try to merge all the block lists into one giant character sheet
        ;; character (assoc (apply merge (map block-strip-meta-info-fields block-list))
        ;;                  :meta-block-list block-list
        ;;                  :meta-fileinfo fileinfo
        ;;                  :meta-character-loaded-from filepath)
        ]
    block-list
    ))

(defn write-quest-file
  [quest-states savepath]

  )


;; File format

;; FilePreamble
;; id - id of the current character (16 bytes)
;; tokens block - list of strings
;; quests block - list of quests

(comment

  (with-bindings {#'gd-edit.structure/*debug* true}
    (load-quest-file "/Users/Odie/dropbox/Public/GrimDawn/main2/_Odie/levels_world001.map/Normal/quests.gdd")
    )

  (def t
    (time
     (load-quest-file "/Users/Odie/dropbox/Public/GrimDawn/main2/_Odie/levels_world001.map/Normal/quests.gdd")))

 )
