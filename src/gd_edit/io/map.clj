(ns gd-edit.io.map
  (:require [gd-edit.io.arc :as arc]
            [clojure.java.io :as io]
            [gd-edit.utils :as u]
            [gd-edit.structure :as s]
            [gd-edit.io.wrl :as wrl]
            [gd-edit.io.arc :as arc])
  (:import [java.nio ByteBuffer]))

(defn get-bytes
  [^ByteBuffer bb byte-count]
  (let [buf (byte-array byte-count)]
    (.get bb buf)
    buf))

(defn advance-bytes
  [^ByteBuffer bb byte-count]
  (.position bb (+ (.position bb) byte-count)))

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

(defn print-bb-info
  [bb]
  (println "position:" (.position bb))
  (println "capacity:" (.capacity bb))
  (println "limit:" (.limit bb))
  (println "remaining:" (.remaining bb))
  )

(defn bytebuffer-limit-to-section
  [bb section-header]
  (.position bb (:offset section-header))
  (.limit bb (+ (:length section-header) (:offset section-header))))

(defn slice-bytebuffer
  "Create a new view of the given byte buffer."
  [bb offset length]
  (doto (.slice bb)
    (.order (.order bb))
    (.position offset)
    (.limit (+ offset length))))


(defn sector-end-pos
  [sector-header]
  (+ (:offset sector-header) (:length sector-header)))

(comment

  (load-section-headers "/Users/Odie/tmp/Levels/world001.map")


  (let [filepath "/Users/Odie/tmp/Levels/world001.map"

        headers (->> (load-section-headers filepath)
                     (u/hashmap-with-keys :type))

        bb (u/mmap filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        shrines (when-let [header (:instance headers)]
                  (wrl/load-shrines-table (bytebuffer-limit-to-section bb header)))
        rift-gates (when-let [header (:instance headers)]
                     (wrl/load-rift-gates-table (bytebuffer-limit-to-section bb header)))]

    (u/collect-as-map shrines rift-gates))

  (let [filepath "/Volumes/Skyrim/SteamLibrary/steamapps/common/Grim Dawn/resources/Levels.arc"
        headers (arc/load-record-headers filepath)

        bb (u/mmap filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        ;; record-headers (->>  (load-record-headers bb header)
        ;;                      (map (fn [record-header]
        ;;                             {:recordname (load-record-filename bb header record-header)
        ;;                              :record-header record-header}))
        ;;                      (filter #(str/ends-with? (:recordname %) ".tex"))
        ;;                      )
        ]

    ;; (->> record-headers
    ;;      (map (fn [record-header]
    ;;             (let [record-bb (as-> (load-record bb header (:record-header record-header)) $
    ;;                               (ByteBuffer/wrap $)
    ;;                               (.order $ java.nio.ByteOrder/LITTLE_ENDIAN))]

    ;;               (assoc (load-fn record-bb)
    ;;                      :recordname (:recordname record-header))))))
    headers
    )

  )
