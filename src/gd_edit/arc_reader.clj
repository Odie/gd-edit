(ns gd-edit.arc-reader
  (:require [gd-edit.structure :as s]
            [gd-edit.utils :as utils])
  (:import  [java.nio ByteBuffer]
            [java.nio.file Path Paths Files FileSystems StandardOpenOption]
            [java.nio.channels FileChannel]
            [net.jpountz.lz4 LZ4Factory])
  (:gen-class))

(def arc-header
  (s/ordered-map
   :magic :int32
   :version :int32
   :file-entries :int32
   :data-records :int32
   :record-table-size :int32
   :string-table-size :int32
   :record-table-offset :int32))

(def arc-record-header
  (s/ordered-map
   :entry-type :int32
   :offset :int32
   :compressed-size :int32
   :decompressed-size :int32
   :decompressed-hash :int32  ;; Adler32 hash of the decompressed file bytes
   :filetime :int64
   :file-parts :int32
   :first-part-index :int32
   :string-entry-length :int32
   :string-entry-offset :int32))

(def arc-file-part-header
  (s/ordered-map
   :offset :int32
   :compressed-size :int32
   :decompressed-size :int32))

(defn- load-header
  [^ByteBuffer bb]

  (.position bb 0)

  (let [header (s/read-struct arc-header bb)]
    (when (or (not= (:magic header) 0x435241) (not= (:version header) 0x3))
      (throw  (Throwable. (str "I don't understand this ARC format!"))))

    header))


(defn- load-record-headers
  [^ByteBuffer bb header]

  (.position bb (+ (:record-table-offset header)
                   (:record-table-size header)
                   (:string-table-size header)))

  (println bb)
  (loop [i 0
         limit (:file-entries header)
         accum []]

    (if (>= i limit)
      accum

      (recur (inc i)
             limit
             (conj accum (s/read-struct arc-record-header bb))))))

(defn load
  [filepath]

  )

#_(time  (load "/Users/Odie/Dropbox/Public/GrimDawn/resources/text_en.arc"))
#_(def f (utils/mmap "/Users/Odie/Dropbox/Public/GrimDawn/resources/text_en.arc"))
#_(.order f java.nio.ByteOrder/LITTLE_ENDIAN)
#_(def h (load-header f))
#_(time (def dt (load-record-headers f h)))
#_(time (def rt (load-db-records f h st)))
#_(time (def st (load-db-string-table f h)))
