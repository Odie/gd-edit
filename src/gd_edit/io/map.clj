(ns gd-edit.io.map
  (:require [gd-edit.io.arc :as arc]
            [clojure.java.io :as io]
            [gd-edit.utils :as u]
            [gd-edit.structure :as s]
            [gd-edit.io.wrl :as wrl]
            [gd-edit.io.arc :as arc]
            [gd-edit.io.arc-record-reader :as arc-record-reader]
            [gd-edit.io.core :as io.core])
  (:import [java.nio ByteBuffer]))

(defn get-bytes
  [^ByteBuffer bb byte-count]
  (let [buf (byte-array byte-count)]
    (.get bb buf)
    buf))

(defn advance-bytes
  [reader byte-count]
  (io.core/set-position reader
                        (+ (io.core/get-position reader)
                           byte-count)))

(def FilePreamble
  (s/struct-def
   :magic   (s/string :ascii :length 3)
   :version :byte))

(def SectionHeader
  (s/struct-def
   :type   :int32
   :length :int32))

(defn validate-preamble
  [preamble]

  (when (or (not= (:magic preamble) "MAP")
            (not= (:version preamble) 8))
    (throw (Throwable. "I don't understand this wrl format!"))))

(def sector-id-map
  {1 :region
	 2 :level
	 3 :impassable
	 5 :entities
	 6 :terrain
	 9 :water
	 0x0A :path-mesh
	 0x10 :spawn-points
	 0x11 :instance
	 0x13 :region-list
	 0x14 :grid-region
	 0x15 :region-iconlist
	 0x16 :old-minimap
	 0x17 :sector-layers
	 0x18 :sector
	 0x19 :minimap
	 0x1A :bitmapTGA
	 0x1B :quest-file
	 0x1C :navigation})

(defn load-section-headers
  [reader]
  (let [preamble (s/read-struct FilePreamble reader)
        _ (validate-preamble preamble)

        ;; Advance 4 more bytes
        ;; Not sure what those 4 bytes are for
        _ (advance-bytes reader 4)]

    (loop [headers []]
      (if-not (io.core/has-remaining? reader)
        headers
        (let [header (-> (s/read-struct SectionHeader reader)
                         (update :type sector-id-map)
                         (assoc :offset (io.core/get-position reader)))]
          (io.core/set-position reader (+ (:offset header) (:length header)))
          (recur (conj headers header)))))))

(defn load-section-headers-from-file
  [filepath]
  (let [bb (u/mmap filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        preamble (s/read-struct FilePreamble bb)
        _ (validate-preamble preamble)

        ;; Advance 4 more bytes
        ;; Not sure what those 4 bytes are for
        _ (advance-bytes bb 4)]

    (loop [headers []]
      (if-not (.hasRemaining bb)
        headers
        (let [header (-> (s/read-struct SectionHeader bb)
                         (update :type sector-id-map)
                         (assoc :offset (.position bb)))
              offset (:offset header)]
          (.position bb (+ (:offset header) (:length header)))
          (recur (conj headers header)))))))


(defn section-end-pos
  [section-header]
  (+ (:offset section-header) (:length section-header)))

(defn bytebuffer-limit-to-section
  [bb section-header]
  (.position bb (:offset section-header))
  (.limit bb (section-end-pos section-header)))

(defn reader-limit-to-section
  [reader section-header]
  (io.core/reader-slice reader
                        (:offset section-header)
                        (section-end-pos section-header)))

(defn slice-bytebuffer
  "Create a new view of the given byte buffer."
  [bb offset length]
  (doto (.slice bb)
    (.order (.order bb))
    (.position offset)
    (.limit (+ offset length))))


(defn load-shrines-and-rift-gates
  [filepath]

  (let [reader (arc-record-reader/arc-record-reader filepath "world001.map")

        headers (->> (load-section-headers reader)
                     (u/hashmap-with-keys :type))

        _ (io.core/set-position reader 0)

        shrines (when-let [header (:instance headers)]
                  (wrl/load-shrines-table (reader-limit-to-section reader header)))
        rift-gates (when-let [header (:instance headers)]
                     (wrl/load-rift-gates-table (reader-limit-to-section reader header)))]

    (u/collect-as-map shrines rift-gates)))

(comment

  (load-section-headers "/Users/Odie/tmp/Levels/world001.map")

  (u/timed-readable
   (load-shrines-and-rift-gates
    "/Volumes/Skyrim/SteamLibrary/steamapps/common/Grim Dawn/resources/Levels.arc"))

  )
