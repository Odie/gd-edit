(ns gd-edit.core
  (:require [clojure.java.io :as io]
            [gloss.core :as gloss]
            [gloss.io])
  (:import [java.nio.file Path Paths Files FileSystems]
           [java.nio.file StandardOpenOption]
           [java.nio.channels FileChannel])
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def arz-header-def
  (gloss/ordered-map
   :unknown :uint16-le
   :version :uint16-le
   :record-table-start   :uint32-le
   :record-table-size    :uint32-le
   :record-table-entries :uint32-le
   :string-table-start   :uint32-le
   :string-table-szie    :uint32-le))


(gloss/defcodec arz-header-codec arz-header-def)

(defn read-bytes
  [file count]

  ;; Create a buffer with the right count
  (let [buf (byte-array count)
        byte-buffer (java.nio.ByteBuffer/allocate (* 8 1024))

        ;; Read the specified number of bytes
        n (.get byte-buffer buf 0 count)]

    ;; If we were able to read the correct number of bytes
    ;; return it
    buf
    ))

(defn mmap
  [filepath]

  (with-open [db-file (java.io.RandomAccessFile. filepath "r")]
    (let [file-channel (.getChannel db-file)
          file-size (.size file-channel)]

          (.map file-channel java.nio.channels.FileChannel$MapMode/READ_ONLY 0 file-size)
          )))

(defn- load-db-header
  [^java.nio.ByteBuffer bb]
  
  (.position bb 0)
  (gloss.io/decode arz-header-codec bb false))

(defn load-game-db
  [filepath]

  ;; Open the database file
  (let [bb (mmap filepath)

        ;; Read and parse the header
        header (load-db-header bb)

        ;; Read in all the file records
        ]

        ;; Read in the individual file parts

    header

    ))

#_(load-game-db "/Users/Odie/Dropbox/Public/GrimDawn/database/database.arz")
