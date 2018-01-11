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


(defn write-task-block
  [bb data context]

  (gdc/write-int! bb (:id1 data) context)
  (s/write-struct UID bb (:id2 data) context)
  (gdc/write-block bb (assoc data :meta-block-id 0) context {0 TaskInnerBlock}))


(def Task
  (gdc/merge-meta
   (s/ordered-map
    :static/type :task
    :id1 :int32
    :id2 UID

    ;; :state :int32
    ;; :in-progress :byte
    ;; :objectives (s/variable-count :int32
    ;;                               :read-length-fn objectives-count
    ;;                               :skip-write-length true)
    )
   {:struct/read read-task-block
    :struct/write write-task-block}))


(def TaskListBlock
  (s/ordered-map
   :static/type :task-list-block
   :tasks (s/variable-count Task)))


(defn read-quest-single
  [bb context]

  (let [data {:id1 (gdc/read-int! bb context)
              :id2 (s/read-struct UID bb context)}
        tasks (read-block bb context {0 TaskListBlock})]

    (assoc data :tasks (:tasks tasks))))

(defn write-quest-single
  [bb data context]

  (gdc/write-int! bb (:id1 data) context)
  (s/write-struct UID bb (:id2 data) context)
  (gdc/write-block bb (assoc data :meta-block-id 0) context {0 TaskListBlock}))

(def Quest
  (gdc/merge-meta
   (s/ordered-map
    :static/type :quest
    :id1 :int32
    :id2 UID
    :tasks (s/variable-count Task))

   {:struct/read read-quest-single
    :struct/write write-quest-single}))


;; Quests block
(def Block11
  ^{:known-versions #{3}}
  (s/ordered-map
   :version :int32
   :quests (s/variable-count Quest)))

(def block-specs
  {10 Block10
   11 Block11})

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
         block-spec (get block-spec-overrides id)

         ;; If neither a block spec or a custom read function can be found...
         ;; We don't know how to read this block
         _ (if (nil? block-spec)
             (throw (Throwable. (str"Don't know how to read block " id))))

         ;; Try to read the block
         ;; If a custom read function was provided, use that
         ;; Otherwise, try to read using a block spec
         block-data (s/read-struct block-spec bb context {})

         ;; Verify that this is a block version we understand
         _ (if-let [known-versions (:known-versions (meta block-spec))]
             (assert (contains? known-versions (:version block-data))))
         ]

     ;; Verify we have the correct enc-state at this point
     (read-and-verify-block-end bb context id expected-end-position)

     ;; Pop from the block stack
     (swap! context update-in [:block-stack] pop)

     (assoc block-data :meta-block-id id))))

(def ^{:private true} seed-mask 0x55555555)

(defn load-quest-file
  [filepath]

  (let [bb ^ByteBuffer (utils/file-contents filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        seed (bit-xor (Integer/toUnsignedLong (.getInt bb)) seed-mask)
        enc-table (gdc/generate-encryption-table seed)
        enc-context (gdc/make-enc-context seed enc-table)

        preamble (s/read-struct FilePreamble bb enc-context)
        _ (validate-preamble preamble)

        id (s/read-struct UID bb enc-context)

        ;; Keep reading more blocks until we've reached the end of the file
        block-list (->> (loop [block-list (transient [])]
                          (if (= (.remaining bb) 0)
                            (persistent! block-list)

                            (recur (conj! block-list (read-block bb enc-context block-specs)))))

                        ;; Append the header block when we're done reading
                        (into []))

        fileinfo {:seed seed
                  :preamble preamble
                  :id id}]
    ;; Try to merge all the block lists into one giant map
    (assoc (apply merge (map gdc/block-strip-meta-info-fields block-list))
           :meta-block-list block-list
           :meta-fileinfo fileinfo
           :meta-loaded-from filepath)))

(defn write-quest-file
  [quest-states savepath]

  (let [bb (ByteBuffer/allocate (* 512 1024))
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        ;; Grab the original block list
        ;; This was kept when we loaded the save file in the order
        ;; the blocks were read
        block-list (:meta-block-list quest-states)

        ;; Create a new block list when the updated contents
        ;; The character sheet was created using the block-list by merging
        ;; all blocks into a giant dictionary.
        ;; To re-create the block list from a character sheet then, we need to
        ;; update the top level kv pair of every block to the current value in
        ;; the character sheet.
        updated-block-list (gdc/update-block-list block-list quest-states)

        fileinfo (:meta-fileinfo quest-states)

        seed (:seed fileinfo)
        enc-table (gdc/generate-encryption-table seed)
        enc-context (gdc/make-enc-context seed enc-table {:direction :write})]

    (.putInt bb (bit-xor seed seed-mask))
    (s/write-struct FilePreamble bb (:preamble fileinfo) enc-context)
    (s/write-struct UID bb (:id fileinfo) enc-context)

    (doseq [block (->> updated-block-list
                       (filter #(not= (:meta-block-id %1) :header)))]
      (gdc/write-block bb block enc-context block-specs))

    (.flip bb)

    (clojure.java.io/make-parents savepath)
    (gdc/write-to-file bb savepath)))


(comment

  (let [src (u/expand-home"~/Dropbox/Public/GrimDawn/main2/_Odie/levels_world001.map/Normal/quests.gdd")
        dst "/tmp/gd/quest.gdd"

        quest-states (load-quest-file src)]

    (write-quest-file quest-states dst)

    (if (= (u/md5-file src) (u/md5-file dst))
      "Success!"
      "Nope, try again!"))

 )
