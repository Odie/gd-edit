(ns gd-edit.arc-reader
  (:require [gd-edit.structure :as s]
            [gd-edit.utils :as utils]
            [clojure.java.io :as io]
            [clojure.string :as string])
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


(defn read-bytes
 "Retrieve the next 'count' bytes from the byte buffer and return it in a new byte array"

 [^ByteBuffer bb count]
 (let [buffer (byte-array count)]
   (.get bb buffer 0 count)
   buffer))

(defn- load-record-filename
  [^ByteBuffer bb header record-header]

  (.position bb (+ (:record-table-offset)
                   (:record-table-size)
                   (:string-entry-offset)))

  (String. (read-bytes bb (:string-entry-length record-header)) "US-ASCII"))


(defn write-to-stream
  "Write the entire bytes array into the stream"
  [^java.io.OutputStream stream bytes]
  (.write stream bytes 0 (count bytes)))


(defn- load-record-file-part
  [^ByteBuffer bb header record-header i]

  ;; Magic number "12" is the size of the arc-file-part on disk
  (.position bb (-> (:first-part-index record-header)
                    (+ i)
                    (* 12)
                    (+ (:record-table-offset header))))

  ;; Read the file part header
  (let [part-header (s/read-struct arc-file-part-header bb)

        ;; compressed-size (:compressed-size part-header)
        ;; decompressed-size (:decompressed-size part-header)
        {:keys [compressed-size decompressed-size]} part-header

        ;; Goto where the file part is
        _ (.position bb (:offset part-header))

        ;; Read the contents
        compressed-bytes (read-bytes bb compressed-size);
        ]

    ;; If this file part doesn't need decompression, we're done...
    (if (= compressed-size decompressed-size)
      compressed-bytes

      ;; Decompress the contents...
      (let [decompressed-bytes (byte-array decompressed-size)]

        ;; Grab a decompressor
        (-> (.fastDecompressor (LZ4Factory/fastestInstance))

            ;; Decompress the data
            (.decompress compressed-bytes 0  decompressed-bytes 0 decompressed-size))

        decompressed-bytes))))


(defn- load-record
  [^ByteBuffer bb header record-header]

  (.position bb (:offset record-header))

  (let [{:keys [compressed-size decompressed-size]} record-header]

    ;; If this is a single part file and no decompression is needed, read and return the contents
    (if (and (= (:entry-type record-header) 1) (= compressed-size decompressed-size))
      (read-bytes bb compressed-size)

      ;; Otherwise, we need to read out the individual parts and decompress them
      (let [output-stream (java.io.ByteArrayOutputStream.)]
        (loop [i 0
               limit (:file-parts record-header)]

          ;; Terminate or read one more file part
          (if (>= i limit)
            nil

            (do
              (write-to-stream output-stream (load-record-file-part bb header record-header i))
              (recur (inc i) limit))))

        ;; We've read and decompressed all the file parts now
        ;; Return the contents as a byte array
        (.toByteArray output-stream)
        ))))


(defn- load-record-headers
  [^ByteBuffer bb header]

  (.position bb (+ (:record-table-offset header)
                   (:record-table-size header)
                   (:string-table-size header)))

  (loop [i 0
         limit (:file-entries header)
         accum []]

    (if (>= i limit)
      accum

      (recur (inc i)
             limit
             (conj accum (s/read-struct arc-record-header bb))))))


(defn- localization-file->hashmap
  [bytes]
  (let [lines (line-seq (io/reader bytes))]

    (reduce
     (fn[accum line]
       ;; In the localization files, a = sign is used to separate the key and the value
       ;; Can we find a "=" sign?
       (let [split-index (string/index-of line "=")]

         ;; If we can't find a = sign, stop processing this line
         (if (= nil split-index)
           accum

           ;; We did find a = sign, extract the key and value then add them to the map
           (assoc accum
                  (string/trim (subs line 0 split-index))
                  (string/trim (subs line (inc split-index)))))
       ))
     {}
     lines)))


(defn load-localization-table
  [filepath]

  (let [bb (utils/mmap filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)
        header (load-header bb)
        record-headers (load-record-headers f h)]

    (->> record-headers
         (map (fn [record-header]
                (localization-file->hashmap
                 (load-record bb header record-header))))
         (reduce into {}))))

#_(time (load-localization-table "/Users/Odie/Dropbox/Public/GrimDawn/resources/text_en.arc"))
#_(def l (load-localization-table "/Users/Odie/Dropbox/Public/GrimDawn/resources/text_en.arc"))
#_(def f (utils/mmap "/Users/Odie/Dropbox/Public/GrimDawn/resources/text_en.arc"))
#_(.order f java.nio.ByteOrder/LITTLE_ENDIAN)
#_(def h (load-header f))
#_(time (def dt (load-record-headers f h)))
#_(time (def r (load-record f h (nth dt 0))))
