(ns gd-edit.utils
  (:require [clojure
             [set :refer [intersection]]
             [string :as str]]
            [jansi-clj.core :refer :all]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [taoensso.timbre :as t]
            [clojure.walk :refer [postwalk-replace]])
  (:import java.nio.ByteBuffer
           java.nio.channels.FileChannel
           java.nio.file.Paths
           java.io.File
           [java.security MessageDigest]))

(defmacro plet [bindings & body]
  (let [bents (partition 2 (destructure bindings))
        smap (into {} (map (fn [[b _]]
                             [b `(deref ~b)])
                           bents))
        bindings (vec (mapcat (fn [[b v]]
                                [b `(future ~(postwalk-replace smap v))])
                              bents))]
    `(let ~bindings
       ~@(postwalk-replace smap body))))

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
  (cond
    (number? s)
    (format "0x%02x " s)

    (or (string? s)
        (seq? s))
    (apply str
           (map #(format "0x%02x " (byte %)) s))))

(defmacro try-or
  "Try to evaluate to the try-body. If some kind of exception is throw, evaluate to the or-body."
  [try-body or-body]

  `(try
     ~try-body
     (catch Exception e#
       ~or-body)))

(defn file-magic
  "Returns a long with lower 32 bits suitable for use as a file magic header."
  [s]

  (let [bytes (map byte s)]
    (bit-or
     (bit-shift-left (nth bytes 3) 24)
     (bit-shift-left (nth bytes 2) 16)
     (bit-shift-left (nth bytes 1) 8)
     (nth bytes 0))))

;;------------------------------------------------------------------------------
;; Logging related functions
;;------------------------------------------------------------------------------
(defmacro log-exp
  [sexp]

  `(t/debug (format "%s => %s" (quote ~sexp) (prn-str ~sexp))))

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

(defmacro time-with-msg
  "Runs the given 'sexp'. When done, print the 'msg' and the run time in seconds."
  [msg sexp]

   `(let [[duration# result#] (timed ~sexp)]
      (println ~msg "in" (nanotime->secs duration#) "seconds")
      result#))

;;------------------------------------------------------------------------------
;; String comparison
;;------------------------------------------------------------------------------
(defn case-insensitive=
  "Check if str2 can be found in `a-str`"
  [a-str substring]

  (= (str/lower-case (str a-str)) (str/lower-case (str substring))))

(defn case-insensitive-match
  "Check if `substring` can be found in `a-str`"
  [a-str substring]

  (str/includes? (str/lower-case (str a-str)) (str/lower-case (str substring))))

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

(defn components->filepath-unix-style
  [components]

  (str/join "/" components))

(defn last-path-component
  [path]

  (last (str/split path #"[/\\]")))

(defn path-exists?
  [path]

  (if (and (not (nil? path))
           (.exists (io/file path)))
    true
    false))

(defn path-exists-or-nil
  [file-path]

  (when (.exists file-path)
    file-path))

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

(defn collect-values-with-key-prefix
  [coll key-prefix]

  (->> coll
       (filter (fn [kv]
                 (str/starts-with? (key kv) key-prefix)))
       (reduce #(conj %1 (val %2)) [])))

(defn slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defn md5
  [bytes]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm bytes)]
    (format "%032x" (BigInteger. 1 raw))))

(defn md5-file
  [file-path]
  (md5 (slurp-bytes file-path)))


;;------------------------------------------------------------------------------
;; Tree node collection utility
;;------------------------------------------------------------------------------
(defn node-children
  [node]

  (cond
    (map? node) (seq node)
    (or (vector? node)
        (seq? node)) (map-indexed vector node)
    :else nil))


(defn- collect-walk-entire-tree-
  "
  Returns a list of [path item] pairs where the item is accepted by the predicate.

  This function will walk the entire tree and examine every single node regardless of
  whether a parent node has been collected.
  "
  [node         ;; current node being processed
   predicate    ;; answers if we want to collect this node or not
   path]        ;; indicates the the path of this node from root

  ;; Collect this node if predicate says to
  (let [results (when (predicate node)
                  [[path node]])]

    ;; Collect any child items that matches the predicate
    ;; If there are children to visit...
    (if-let [child-pairs (node-children node)]

      ;; Visit each, collect the results into a list, merge it into `results`
      (into (or results [])
            (mapcat (fn [[key child]]
                      (collect-walk-entire-tree- child predicate (conj path key)))
                    child-pairs))

      ;; If there are no children to visit, just return the results, which may or may-not
      ;; contain the current node
      results)))


(defn collect-walk-entire-tree
  "
  Walk the given tree and collect items that matches the predicate.

  This function will walk the entire tree and examine every single node regardless of
  whether a parent node has been collected.
  "
  [predicate tree]

  (collect-walk-entire-tree- tree predicate []))

(defn- collect-walk-first-in-branch-
  "
  Returns a list of [path item] pairs where the item is accepted by the predicate.

  This is similar to `collect-walk-entire-tree`. However, this function stops exploring
  further down a branch when it finds a node it wants to collect.
  "
  [node         ;; current node being processed
   predicate    ;; answers if we want to collect this node or not
   path]        ;; indicates the the path of this node from root

  ;; Should this node be collected?
  (if (predicate node)
    ;; If so, just return the current node and do not explore further down this branch.
    [[path node]]

    ;; Otherwise...
    ;; Collect any child items that matches the predicate
    ;; If there are children to visit...
    (when-let [child-pairs (node-children node)]

      ;; Visit each, collect the results into a list, merge it into `results`
      (into []
            (mapcat (fn [[key child]]
                      (collect-walk-first-in-branch- child predicate (conj path key)))
                    child-pairs)))))

(defn collect-walk-first-in-branch
  "
  Walk the given tree and collect items that matches the predicate

  This function will not explore nodes where the parent has been collected.
  "
  [predicate tree]

  (collect-walk-first-in-branch- tree predicate []))

(defn collect-walk
  "Walk the given tree and collect items that matches the predicate

  The tree can be a mix of map, vectors, or a sequence. We send every reachable
  node to the predicate. For each matched node, we collect the path where the item
  can be found as well as the value itself.

  This allows the caller to find all nodes that matches a condition as well as
  perform updates into each of the locations later on."
  ([predicate tree]
   (collect-walk predicate :entire-tree :entire-tree))

  ([predicate walk-type tree]

   (cond
     (= walk-type :entire-tree)
     (collect-walk-entire-tree- tree predicate [])

     (= walk-type :first-in-branch)
     (collect-walk-first-in-branch- tree predicate [])

     :else
     (throw (Throwable. (str "Unknown collect-walk type: " walk-type))))))
