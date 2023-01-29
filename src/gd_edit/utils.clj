(ns gd-edit.utils
  (:require [clojure
             [set :refer [intersection]]
             [string :as str]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [taoensso.timbre :as t]
            [clojure.walk :refer [postwalk-replace]]
            [me.raynes.fs :as fs]
            [cpath-clj.core :as cp]
            [clj-http.client :as client]
            [clojure.data.json :as json])
  (:import java.nio.ByteBuffer
           java.io.File
           java.io.IOException
           [java.security MessageDigest]))

(def ^:dynamic *suppress-print* false)

(defn print-line
  [& args]
  (when-not *suppress-print*
    (apply println args)))

(defn print-
  [& args]
  (when-not *suppress-print*
    (apply print args)))

(defn newline-
  []
  (when-not *suppress-print*
    (newline)))

(defmacro when-let*
  "Multiple binding version of when-let"
  [bindings & body]
  (if (seq bindings)
    `(when-let [~(first bindings) ~(second bindings)]
       (when-let* ~(vec (drop 2 bindings)) ~@body))
    `(do ~@body)))

(defmacro collect-as-map
  "foo = 1, bar = 2.  (to-map foo bar) ==> {:foo 1 :bar 2}"
  [& vs]
  `(let [ks# (map keyword '~vs)]
     (zipmap ks# [~@vs])))

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

(defmacro swallow-exceptions
  [& body]
  `(try ~@body (catch Exception e#)))

(defn file-magic
  "Returns a long with lower 32 bits suitable for use as a file magic header."
  [s]

  (let [bytes (map byte s)]
    (bit-or
     (bit-shift-left (nth bytes 3 0) 24)
     (bit-shift-left (nth bytes 2 0) 16)
     (bit-shift-left (nth bytes 1 0) 8)
     (nth bytes 0))))

(defn call-method
  [obj method-name & args]
  (let [m (first (filter (fn [x] (.. x getName (equals method-name)))
                         (.. obj getClass getDeclaredMethods)))]
    (. m (setAccessible true))
    (. m (invoke obj (into-array Object args)))))


(defn strip-leading-indent
  [text]

  (let [t (str/split-lines text)
        safe-subs (fn [s start]
                    (if (<= (count s) start)
                      ""
                      (subs s start)))
        indent-level (str/index-of (second t)
                                   (re-find #"[^\s-]" (second t)))]
    (str/join "\n" (concat (list (first t)) (map #(safe-subs % indent-level) (rest t))))))

(defn subs+
  "Like subs, but allows negative values to indicate `start` and `end`"
  ([s start]
   (subs+ s start (count s)))
  ([s start end]
   (let [start (if (neg? start)
                 (+ (count s) start)
                 start)
         end (if (neg? end)
               (+ (count s) end)
               end)]
     (subs s start end))))

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.

  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn hashmap-with-keys
  [keyfn coll]

  (reduce (fn [m item]
            (assoc m (keyfn item) item))
          {}
          coll))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

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

(defmacro log-exceptions-with
  [log-fn & body]

  `(try
     ~@body
     (catch Exception e#
       (~log-fn e#))))

(defmacro print-stack []
  `(doseq [s# (.getStackTrace (Thread/currentThread))]
     (print-line s#)))

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
      (print-line ~msg "in" (nanotime->secs duration#) "seconds")
      result#))

(defn readable-time
  ([time]
   (readable-time time :msec))
  ([time unit]

   ;; The conversion table tells us when a unit should be promoted to the next larger unit.
   (let [conversion-table {:ns [1000 :us]
                           :us [1000 :msec]
                           :msec [1000 :sec]
                           :sec [60 :min]
                           :min [60 :hour]
                           :hour [24 :day]
                           :day nil}
         result (loop [accum (list)
                       time time
                       unit unit]

                  ;; Should we promote this to the next unit?
                  ;; What is the max this particular unit can hold?
                  (let [[unit-max next-unit] (get conversion-table unit nil)]
                    (cond
                      ;; If there are no more promotion possible, we're done...
                      ;; Append the pending time left over,
                      ;; and return the result
                      (nil? unit-max)
                      (conj accum [time unit])

                      (> time unit-max)
                      (recur
                       (conj accum [(mod time unit-max) unit])
                       (/ time unit-max)
                       next-unit)

                      :else
                      (conj accum [time unit]))))]
     (map (fn[[time unit]] [(int time) unit]) result))))

(defmacro timed-readable
  "Times the execution time of the given expression. Returns a vector of [elapsed-time sexp-result]"
  [sexp]

  `(let [start# (System/nanoTime)
         result# ~sexp
         end# (System/nanoTime)]
     [(readable-time (- end# start#) :ns) result#]))

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

(defn rank-by-similarity
  [target-str str-fn coll]
  (->> coll
       (map #(hash-map :rating (float (string-similarity (str/lower-case (str-fn %)) (str/lower-case target-str)))
                       :item %))
       (sort-by :rating >)
       (into [])))

(defn first-item-is-match?
  [rankings]

  (when (or (>= (get-in rankings [0 :rating]) 0.85)
            (>= (- (get-in rankings [0 :rating]) (get-in rankings [1 :rating])) 0.25))
    true))

;;------------------------------------------------------------------------------
;; Path related functions
;;------------------------------------------------------------------------------
(defn home-dir
  []
  (System/getProperty "user.home"))

(defn expand-home [s]
  (if (.startsWith s "~")
    (str/replace-first s "~" (home-dir))
    s))

(defn path-components
  [path]
  (str/split path #"[/\\]"))

(defn filepath->components
  [path]

  (path-components path))

(defn components->filepath
  [components]

  (str/join File/separator components))

(defn components->filepath-unix-style
  [components]

  (str/join "/" components))

(defn last-path-component
  [path]
  (last (path-components path)))

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

(defn file-extension
  [f]
  (let [filename (str f)]
    (when-let [idx (str/last-index-of filename ".")]
      (subs filename (inc idx)))))

(defn path-basename
  [path]
  (let [path (str path)
        filename (last-path-component path)]
    (when-let [idx (str/last-index-of filename ".")]
      (subs filename 0 idx))))

(defn keywords->path
  [ks]
  (->> ks
       (map #(if (keyword? %)
               (name %)
               %))
       (str/join "/" )))

;;------------------------------------------------------------------------------
;; Core lib extensions
;;------------------------------------------------------------------------------
(def byte-array-type (Class/forName "[B"))

(defn byte-array?
  [obj]
  (= byte-array-type (type obj)))

(defmacro fmt
  "Like 'format' but with string interpolation"
  [^String string]
  (let [-re #"#\{(.*?)\}"
        fstr (str/replace string -re "%s")
        fargs (map #(read-string (second %)) (re-seq -re string))]
    `(format ~fstr ~@fargs)))


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
    (catch Exception _)))

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

  (dotimes [_ indent-level]
    (print- "    ")))

(defn collect-values-with-key-prefix
  [coll key-prefix]
  (->> coll
       (filter (fn [kv]
                 (str/starts-with? (key kv) key-prefix)))
       (map val)
       (into [])))

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
   (collect-walk predicate :entire-tree tree))

  ([predicate walk-type tree]

   (cond
     (= walk-type :entire-tree)
     (collect-walk-entire-tree- tree predicate [])

     (= walk-type :first-in-branch)
     (collect-walk-first-in-branch- tree predicate [])

     :else
     (throw (Throwable. (str "Unknown collect-walk type: " walk-type))))))


(defn human-readable-byte-count
  [byte-count]

  (cond
    (>= byte-count (* 1024 1024 1024 1024))
    (str (quot byte-count (* 1024 1024 1024 1024)) " TB")

    (>= byte-count (* 1024 1024 1024))
    (str (quot byte-count (* 1024 1024 1024)) " GB")

    (>= byte-count (* 1024 1024))
    (str (quot byte-count (* 1024 1024)) " MB")

    (>= byte-count 1024)
    (str (quot byte-count 1024) " KB")

    :else
    (str byte-count " bytes")))


(defn with-idx
  [items]
  (map vector (range) items))


(defn wrap-line [size text]
  (->> (re-seq
        (re-pattern (format "(.{1,%d})( +|$\\n?)|(.{1,%d})" size size))
        (clojure.string/replace text #"\n" " "))
       (map second)))

(defn maybe-int
  [x]
  (if-not (number? x)
    x
    (let [int-x (int x)]
      (if (== int-x x)
        int-x
        x))))

(defn wait-file-stopped-growing
  [f]
  (loop [last-length 0]
    (let [cur-length (.length f)]

      ;; If the current length is the same as the last observed length...
      (if (and (> cur-length 0)
               (= cur-length last-length))

        ;; We'll say that the file has stopped growing...
        :done

        ;; Otherwise, wait a little longer and try again
        (do
          (Thread/sleep 100)
          (recur cur-length))))))

;;------------------------------------------------------------------------------
;; path-seq
;;------------------------------------------------------------------------------
(defprotocol PathSeq
  (path-seq* [form path] "Helper for path-seq"))

(def path-seq-helper
  (fnil conj []))

(extend-protocol PathSeq
  java.util.List
  (path-seq*
   [form path]
   (->> (map-indexed
         (fn [idx item]
           (path-seq* item (path-seq-helper path idx)))
         form)
        (mapcat identity)))

  java.util.Map
  (path-seq*
   [form path]
   (->> (map
         (fn [[k v]]
           (path-seq* v (path-seq-helper path k)))
         form)
        (mapcat identity)))

  java.util.Set
  (path-seq*
   [form path]
   (->> (map
         (fn [v]
           (path-seq* v (path-seq-helper path v)))
         form)
        (mapcat identity)))


  java.lang.Object
  (path-seq* [form path] [[form path]])

  nil
  (path-seq* [_ path] [[nil path]]))

(defn path-seq
  "Returns a sequence of paths into a form, and the elements found at
   those paths.  Each item in the sequence is a map with :path
   and :form keys. Paths are built based on collection type: lists
   by position, maps by key, and sets by value, e.g.

   (path-seq [:a [:b :c] {:d :e} #{:f}])

   ({:path [0], :form :a}
    {:path [1 0], :form :b}
    {:path [1 1], :form :c}
    {:path [2 :d], :form :e}
    {:path [3 :f], :form :f})
   "
  [form]
  (map
   #(let [[form path] %]
      {:path path :val form})
   (path-seq* form nil)))

(defn open-null-stream
  []
  (if (running-windows?)
    (java.io.FileOutputStream. "NUL")
    (java.io.FileOutputStream. "/dev/null")))

(defn clamp
  [val minimum maximum]
  (->> val
       (max minimum)
       (min maximum)))

(defn starts-with-insensitive?
  [s substring]

  (str/starts-with? (str/lower-case s) (str/lower-case substring)))

(defn reverse-map
  [hmap]

  (->> hmap
       (map (fn [[k v]] [v k]))
       (into {})))

(defn lower-cased-keys
  [hmap]

  (->> hmap
       (map (fn [[k v]] [(str/lower-case k) v]))
       (into {})))

(defn positions
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn first-match-position
  [pred coll]
  (some (fn [[idx x]]
          (when (pred x)
            idx))
        (indexed coll)))

(defn replace-path-extension
  [path new-ext]

  (-> (io/file (fs/parent path) (fs/base-name path true))
      (str new-ext)))

(defn replace-path-basename
  [path new-basename]

  (-> (io/file (fs/parent path) new-basename)
      (str (fs/extension path))))

(defn path-sibling
  [path sibling-name]

  (-> (io/file path)
      fs/parent
      (io/file sibling-name)
      str))

(defn copy-resource-files-recursive
  "Recursively copies files from a resource directory (which might be in a jar file)
  into the target directory."
  [src-resource-dir target-dir]

  (let [src-files (cp/resources (io/resource src-resource-dir))]

    (doseq [[path uris] src-files]
      (let [uri (first uris)
            relative-path (subs path 1)
            output-file (io/file target-dir relative-path)]
        (->> output-file
             (.getParentFile)
             (.mkdirs)
             )
        (with-open [in (io/input-stream uri)]
          (io/copy in output-file))))))

(defn select-keys-pred
  [m pred]
  (->> m
       (filter (fn [[k v]] (pred k)))))

(defn key-by
  [f coll]

  (reduce (fn [accum item]
            (assoc accum (f item) item))
          {}
          coll))

(defn println-passthrough
  [x]
  (println x)
  x)

(defn fetch-url 
  [url-str]
  (let [response (clj-http.client/get url-str
                                      {:headers {"User-Agent" "curl/7.43.0"}})]
    (if (not= (response :status) 200)
      (throw (IOException. (str "Got response status:" (response :status))))

      (response :body))))

(defn fetch-json-from-url
  [url-str]
  (json/read-json (fetch-url url-str) true))

(defn load-json-file
  [filepath]
  (json/read-json (slurp (expand-home filepath)) true))