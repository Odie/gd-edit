(ns gd-edit.utils
  (:require [clojure
             [set :refer [intersection]]
             [string :as string]]
            [gd-edit.utils :as u]
            [jansi-clj.core :refer :all]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import java.nio.ByteBuffer
           java.nio.channels.FileChannel
           java.nio.file.Paths))

(defn mmap
  [filepath]

  (with-open [db-file (java.io.RandomAccessFile. filepath "r")
              file-channel (.getChannel db-file)]
    (let [file-size (.size file-channel)]

      (.map file-channel java.nio.channels.FileChannel$MapMode/READ_ONLY 0 file-size))))

(defn file-contents
  [filepath]

  (with-open [file-channel (.getChannel (java.io.RandomAccessFile. filepath "r"))]
    (let [bb (ByteBuffer/allocate (.size file-channel))]
      (.read file-channel bb)
      (.rewind bb))))

(defn hexify [s]
  (apply str
         (map #(format "%02x " (byte %)) s)))

(defmacro timed
  "Times the execution time of the given expression. Returns a vector of [elapsed-time sexp-result]"
  [sexp]

  `(let [start# (System/nanoTime)
         result# ~sexp
         end# (System/nanoTime)]
    [(- end# start#) result#]))

(defn nanotime->secs
  "Given duration in nanoseconds, return the duration in seconds"
  [time]
  (/ (float time) 1000000000))

(defn case-insensitive-match
  "Check if str2 can be found in str1"
  [str1 str2]

  (.contains (string/lower-case (str str1)) (string/lower-case (str str2))))

(def ci-match case-insensitive-match)

(defn expand-home [s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (System/getProperty "user.home"))
    s))

(defmacro doseq-indexed [index-sym [item-sym coll] & body]
  `(doseq [[~item-sym ~index-sym]
           (map vector ~coll (range))]
     ~@body))

(def byte-array-type (Class/forName "[B"))

(defn bigrams [s]
  (->> (clojure.string/split s #"\s+")
       (mapcat #(partition 2 1 %))
       (set)))

(defn string-similarity [a b]
  (let [a-pairs (bigrams a)
        b-pairs (bigrams b)
        total-count (+ (count a-pairs) (count b-pairs))
        match-count (count (intersection a-pairs b-pairs))
        similarity (/ (* 2 match-count) total-count)]
    similarity))

(defn byte-array?
  [obj]
  (= byte-array-type (type obj)))

(defn working-directory
  []
  (System/getProperty "user.dir"))

(defn settings-file-path
  []
  (.getAbsolutePath (io/file (working-directory) "settings.edn")))

(defn load-settings
  []
  (try
    (edn/read-string (slurp (settings-file-path)))
    (catch Exception e)))

(defn write-settings
  [settings]
  (spit (settings-file-path) (pr-str settings)))

(defn path-exists
  [path]

  (if (and (not (nil? path))
           (.exists (io/file path)))
    true
    false))
