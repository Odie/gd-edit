(ns gd-edit.utils
  (:import  [java.nio ByteBuffer]
            [java.nio.channels FileChannel]))

(defn mmap
  [filepath]

  (with-open [db-file (java.io.RandomAccessFile. filepath "r")]
    (let [file-channel (.getChannel db-file)
          file-size (.size file-channel)]

      (.map file-channel java.nio.channels.FileChannel$MapMode/READ_ONLY 0 file-size))))

(defn hexify [s]
  (apply str
         (map #(format "%02x " (byte %)) s)))
