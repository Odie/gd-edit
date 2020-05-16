(ns gd-edit.io.wrl
  (:require [gd-edit.structure :as s]
            [gd-edit.utils :as u]
            [clojure.pprint :refer [pprint]]
            [kmp-search.core :as kmp]
            [gd-edit.io.gdc :as gdc])

  (:import  [java.nio ByteBuffer ByteOrder]))

(def FilePreamble
  (s/struct-def
   :magic   (s/string :ascii :length 3)
   :version :byte))

(defn validate-preamble
  [preamble]

  (when (or (not= (:magic preamble) "WRL")
            (not= (:version preamble) 9))
    (throw (Throwable. "I don't understand this wrl format!"))))


(def SectionHeader
  (s/struct-def
   :header-bytes1 (s/string :bytes :length 4)
   :header-bytes2 (s/string :bytes :length 4)
   :section-name1 (s/string :ascii)
   :section-name2 (s/string :ascii)))

(def UIDTable
  (into SectionHeader
        (s/struct-def
         :table (s/array
                 (s/struct-def
                  :id gdc/UID
                  :record (s/string :ascii)
                  :data (s/string :bytes :length 28))))))

(defn search-buffer
  "returns a vector containing all the offsets where a byte pattern
  appears within a file"
  [^bytes buffer buf-limit ^bytes pattern]
  (loop [context (kmp/context pattern)
         matches []]
    (let [new-context (kmp/search context buffer buf-limit)]
      (if-let [match (kmp/match new-context)]
        (recur new-context (conj matches match))
        matches))))

(defn search-bytebuffer
  "returns a vector containing all the offsets where a byte pattern
  appears within a file"
  [^ByteBuffer bb ^bytes pattern & {:keys [buffer-size] :or {buffer-size 1024}}]
  (let [buffer (byte-array buffer-size)
        copy-data (fn [bb buf buf-size]
                    (let [read-count (Math/min buf-size (.remaining bb))]
                      (.get bb buf 0 read-count)
                      read-count))]
    (loop [context (kmp/context pattern)
           read-count (copy-data bb buffer buffer-size)
           matches []]
      (if (zero? read-count)
        matches
        (let [new-context (kmp/search context buffer read-count)]
          (if-let [match (kmp/match new-context)]
            (recur new-context read-count (conj matches match))
            (recur new-context (copy-data bb buffer buffer-size) matches)))))))

(defn load-wrl-gates-and-shrines2
  [^ByteBuffer bb]

  (.mark bb)
  (let [initial-offset (.position bb)

        rift-gates-offset (first (search-bytebuffer bb
                                                    (byte-array (concat [0x00 0x00 0x00 0x00
                                                                         0x03 0x00 0x00 0x00
                                                                         0x11 0x00 0x00 0x00]
                                                                        (.getBytes "- World Riftgates")))))

        shrines-offset (first (search-bytebuffer bb
                                                 (byte-array (concat [0x00 0x00 0x00 0x00
                                                                      0x03 0x00 0x00 0x00
                                                                      0x12 0x00 0x00 0x00]
                                                                     (.getBytes "-- Devotion Shrine")))))]
    (println "rfo" rift-gates-offset)
    (println "so" shrines-offset)
    (cond-> {}
      rift-gates-offset
      (assoc :rift-gates
             (do
               (.position bb (+ initial-offset rift-gates-offset))
               (->>
                (s/read-struct UIDTable bb)
                :table
                (map #(select-keys % [:id :record])))))

      shrines-offset
      (assoc :shrines
             (do
               (.position bb (+ initial-offset shrines-offset))
               (->>
                (s/read-struct UIDTable bb)
                :table
                (map #(select-keys % [:id :record]))))))
    ))

(defn load-UID-table
  [^ByteBuffer bb pattern]

  ;; (println "position:" (.position bb))
  ;; (println "limit:" (.limit bb))
  ;; (println "remaining:" (.remaining bb))
  (let [initial-offset (.position bb)]
    ;; Can we locate a pattern that marks the beginning of a UID table?
    (when-let [table-offset (first (search-bytebuffer bb pattern))]
      ;; (println "table-offset" table-offset)

      ;; Position the bytebuffer that location
      (.position bb (+ initial-offset table-offset))
      ;; (println "---------")
      ;; (println "position:" (.position bb))
      ;; (println "limit:" (.limit bb))
      ;; (println "remaining:" (.remaining bb))
      ;; (println)

      ;; Read in the table and keep only the :id and :record fields
      (->> (s/read-struct UIDTable bb)
           :table
           (map #(select-keys % [:id :record]))))))

(defn load-shrines-table
  [^ByteBuffer bb]
  (load-UID-table bb
                  (byte-array (concat [0x00 0x00 0x00 0x00
                                       0x03 0x00 0x00 0x00
                                       0x12 0x00 0x00 0x00]
                                      (.getBytes "-- Devotion Shrine")))))

(defn load-rift-gates-table
  [^ByteBuffer bb]
  (load-UID-table bb
                  (byte-array (concat [0x00 0x00 0x00 0x00
                                       0x03 0x00 0x00 0x00
                                       0x11 0x00 0x00 0x00]
                                      (.getBytes "- World Riftgates")))))

(defn load-wrl-gates-and-shrines
  [filepath]

  (let [rift-gates-offset (first (kmp/search-file (byte-array (concat [0x00 0x00 0x00 0x00
                                                                       0x03 0x00 0x00 0x00
                                                                       0x11 0x00 0x00 0x00]
                                                                      (.getBytes "- World Riftgates")))
                                                  filepath))
        shrines-offset (first (kmp/search-file (byte-array (concat [0x00 0x00 0x00 0x00
                                                                    0x03 0x00 0x00 0x00
                                                                    0x12 0x00 0x00 0x00]
                                                                   (.getBytes "-- Devotion Shrine")))
                                               filepath))

        bb ^ByteBuffer (u/file-contents filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)


        ]

    (cond-> {}
      rift-gates-offset
      (assoc :rift-gates
             (do
               (.position bb rift-gates-offset)
               (->>
                (s/read-struct UIDTable bb)
                :table
                (map #(select-keys % [:id :record])))))

      shrines-offset
      (assoc :shrines
             (do
               (.position bb shrines-offset)
               (->>
                (s/read-struct UIDTable bb)
                :table
                (map #(select-keys % [:id :record]))))))))
