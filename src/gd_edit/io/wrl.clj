(ns gd-edit.io.wrl
  (:require [gd-edit.structure :as s]
            [gd-edit.utils :as u]
            [clojure.pprint :refer [pprint]]
            [kmp-search.core :as kmp]
            [gd-edit.io.gdc :as gdc]
            [gd-edit.io.core :as io.core])

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
                  :uid gdc/UID
                  :recordname (s/string :ascii)
                  :data (s/string :bytes :length 28))))))

(defn search-buffer
  "Returns a vector containing all the offsets where a byte pattern
  appears within the `buffer`"
  [^bytes buffer buf-limit ^bytes pattern]
  (loop [context (kmp/context pattern)
         matches []]
    (let [new-context (kmp/search context buffer buf-limit)]
      (if-let [match (kmp/match new-context)]
        (recur new-context (conj matches match))
        matches))))

(defn search-byte-reader
  "returns a vector containing all the offsets where a byte pattern
  appears accessible through `reader`"
  [reader ^bytes pattern & {:keys [buffer-size] :or {buffer-size 1024}}]
  (let [buffer (byte-array buffer-size)
        copy-data (fn [reader buf buf-size]
                    (let [read-count (Math/min buf-size (io.core/get-remaining reader))]
                      (io.core/get-byte-array reader buf 0 read-count)
                      read-count))]
    (loop [context (kmp/context pattern)
           read-count (copy-data reader buffer buffer-size)
           matches []]
      (if (zero? read-count)
        matches
        (let [new-context (kmp/search context buffer read-count)]
          (if-let [match (kmp/match new-context)]
            (recur new-context read-count (conj matches match))
            (recur new-context (copy-data reader buffer buffer-size) matches)))))))

(defn load-UID-table
  [reader pattern]

  (let [initial-offset (io.core/get-position reader)]
    ;; Can we locate a pattern that marks the beginning of a UID table?
    (when-let [table-offset (first (search-byte-reader reader pattern))]

      ;; Position the bytebuffer that location
      (io.core/set-position reader (+ initial-offset table-offset))

      ;; Read in the table and keep only the :id and :record fields
      (->> (s/read-struct UIDTable reader)
           :table
           (map #(select-keys % [:uid :recordname]))))))

(defn load-shrines-table
  [reader]
  (load-UID-table reader
                  (byte-array (concat [0x00 0x00 0x00 0x00
                                       0x03 0x00 0x00 0x00
                                       0x12 0x00 0x00 0x00]
                                      (.getBytes "-- Devotion Shrine")))))

(defn load-rift-gates-table
  [reader]
  (load-UID-table reader
                  (byte-array (concat [0x00 0x00 0x00 0x00
                                       0x03 0x00 0x00 0x00
                                       0x11 0x00 0x00 0x00]
                                      (.getBytes "- World Riftgates")))))
