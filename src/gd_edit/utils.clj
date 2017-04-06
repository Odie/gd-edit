(ns gd-edit.utils
  (:require [clojure
             [set :refer [intersection]]
             [string :as str]]
            [jansi-clj.core :refer :all]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [taoensso.timbre :as t])
  (:import java.nio.ByteBuffer
           java.nio.channels.FileChannel
           java.nio.file.Paths
           java.io.File))

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


;;------------------------------------------------------------------------------
;; Logging related functions
;;------------------------------------------------------------------------------
(defmacro log-exp
  [sexp]

  `(t/debug (format "%s => %s" (quote ~sexp) ~sexp)))

(defmacro log-exceptions
  [& body]

  `(try
     ~@body
     (catch Exception e#
       (println "Caught exception:" (.getMessage e#))
       (clojure.stacktrace/print-stack-trace e#)
       (newline)

       (t/info e#))))

(defmacro print-stack []
  `(doseq [s# (.getStackTrace (Thread/currentThread))]
     (println s#)))

;;------------------------------------------------------------------------------
;; Timing functions
;;------------------------------------------------------------------------------
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

;;------------------------------------------------------------------------------
;; String comparison
;;------------------------------------------------------------------------------
(defn case-insensitive=
  "Check if str2 can be found in str1"
  [str1 str2]

  (= (str/lower-case (str str1)) (str/lower-case (str str2))))

(defn case-insensitive-match
  "Check if str2 can be found in str1"
  [str1 str2]

  (.contains (str/lower-case (str str1)) (str/lower-case (str str2))))

(def ci-match case-insensitive-match)

(defn bigrams [s]
  (->> (str/split s #"\s+")
       (mapcat #(partition 2 1 %))
       (set)))

(defn string-similarity [a b]
  (let [a-pairs (bigrams a)
        b-pairs (bigrams b)
        total-count (+ (count a-pairs) (count b-pairs))
        match-count (count (intersection a-pairs b-pairs))
        similarity (/ (* 2 match-count) total-count)]
    similarity))


;;------------------------------------------------------------------------------
;; Path related functions
;;------------------------------------------------------------------------------
(defn expand-home [s]
  (if (.startsWith s "~")
    (str/replace-first s "~" (System/getProperty "user.home"))
    s))

(defn filepath->components
  [path]

  (str/split path #"[/\\]"))

(defn components->filepath
  [components]

  (str/join File/separator components))

(defn last-path-component
  [path]

  (last (str/split path #"[/\\]")))

(defn path-exists
  [path]

  (if (and (not (nil? path))
           (.exists (io/file path)))
    true
    false))

;;------------------------------------------------------------------------------
;; Core lib extensions
;;------------------------------------------------------------------------------
(defmacro doseq-indexed [index-sym [item-sym coll] & body]
  `(doseq [[~item-sym ~index-sym]
           (map vector ~coll (range))]
     ~@body))

(def byte-array-type (Class/forName "[B"))

(defn byte-array?
  [obj]
  (= byte-array-type (type obj)))

(defmacro fmt
  [^String string]
  "Like 'format' but with string interpolation"
  (let [-re #"#\{(.*?)\}"
        fstr (str/replace string -re "%s")
        fargs (map #(read-string (second %)) (re-seq -re string))]
    `(format ~fstr ~@fargs)
    ))


;;------------------------------------------------------------------------------
;; Environment info
;;------------------------------------------------------------------------------

(defn working-directory
  []
  (System/getProperty "user.dir"))

(defn running-linux?
  []
  (= (System/getProperty "os.name") "Linux"))

(defn running-osx?
  []
  (= (System/getProperty "os.name") "Mac OS X"))

(defn running-nix?
  []
  (or (running-osx?)
      (running-linux?)))

(defn running-windows?
  []
  (str/starts-with? (System/getProperty "os.name") "Windows"))


;;------------------------------------------------------------------------------
;; Settings file
;;------------------------------------------------------------------------------
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


;;------------------------------------------------------------------------------
;; Generic utils
;;------------------------------------------------------------------------------
(defn keyword->str
  [k]
  (if (keyword? k)
    (subs (str k) 1)
    k))

(defn print-indent
  [indent-level]

  (dotimes [i indent-level]
    (print "    ")))
