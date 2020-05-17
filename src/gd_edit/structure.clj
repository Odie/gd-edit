(ns gd-edit.structure
  (:require [clojure.test]
            [clojure.pprint]
            [gd-edit.io.core :as io.core]
            [clojure.java.io :as io])
  (:import  [java.nio ByteBuffer])
  (:gen-class))

(def ^:dynamic *debug* false)

(defn spec-type
  [spec & rest]
  (:struct/type (meta spec)))

(defmulti read-spec spec-type)

(def bytebuffer-reader-fns
  ;; name => primitives spec
  {:byte   [:byte    1 (fn[^ByteBuffer bb & prim-specs] (.get bb))      ]
   :bool   [:byte    1 (fn[^ByteBuffer bb & prim-specs] (.get bb))      ]
   :int16  [:int16   2 (fn[^ByteBuffer bb & prim-specs] (.getShort bb)) ]
   :int32  [:int32   4 (fn[^ByteBuffer bb & prim-specs] (.getInt bb))   ]
   :int64  [:int64   8 (fn[^ByteBuffer bb & prim-specs] (.getLong bb))  ]
   :float  [:float   4 (fn[^ByteBuffer bb & prim-specs] (.getFloat bb)) ]
   :double [:double  4 (fn[^ByteBuffer bb & prim-specs] (.getDouble bb))]})

(def data-reader-fns
  {:byte   [:byte    1 (fn[reader & _] (io.core/get-byte reader))  ]
   :bool   [:byte    1 (fn[reader & _] (io.core/get-byte reader))  ]
   :int16  [:int16   2 (fn[reader & _] (io.core/get-int16 reader)) ]
   :int32  [:int32   4 (fn[reader & _] (io.core/get-int32 reader)) ]
   :int64  [:int64   8 (fn[reader & _] (io.core/get-int64 reader)) ]
   :float  [:float   4 (fn[reader & _] (io.core/get-float reader)) ]
   :double [:double  4 (fn[reader & _] (io.core/get-double reader))]})

(defn atom?
  [x]
  (instance? clojure.lang.Atom x))

(defn sizeof
  "Given a spec, return the byte size it represents"
  [spec & prim-specs]

  ;; Look through all the items in the given spec
  (reduce
   (fn [accum item]
     ;; Does a matching entry exist for the item in the primitives array?
     (let [primitives-spec (prim-specs item)]
       (if primitives-spec
         ;; If so, add the size to the accumulator
         (+ accum (nth primitives-spec 1))

         ;; If not, we've come across a field name. Don't do anything
         accum
         )))

   ;; Start the sum at 0
   0

   ;; Ignore all odd fields, assuming they are fieldnames
   (take-nth 2 (rest spec))))


(defn struct-def?
  [spec]
  (= (-> spec
         meta
         :struct/return-type)
     :map))

(defn- merge-default-rw-fns
  [bb context]
  (letfn [(merge-default  [context]
            (assert (or (map? context) (nil? context)))
            (let [reader-fns
                  (cond (satisfies? io.core/DataReader bb)
                        data-reader-fns
                        :else
                        bytebuffer-reader-fns)]
              (merge-with merge
                          {:rw-fns reader-fns}
                          (or context {}))))]
    (let [context (if (atom? context)
                    context
                    (atom context))]
      (swap! context merge-default)
      context)))

(defn read-struct
  "Spec can also be a single primitive spec. In that case, we'll just read
  a single item from the bytebuffer.

  Spec can be a simple sequence of type specs. In that case, a list values
  that corresponds to each item in the given spec.

  Spec can be a collect with some special meta tags. In those cases, special
  handling will be invoked via the 'read-spec' multimethod.

  The caller can also optionally supply a primitives spec table. If an atom
  supplied, it is assumed that it is being passed some mutable context.
  It will attempt to look up the :spec key to locate the actual specs."
  ([spec ^ByteBuffer bb & [context data rest]]

   (let [context (merge-default-rw-fns bb context)]

     ;; Callers can provide a map or an atom to specify how to read primitives.
     ;; Read in the value using either a supplied read function or
     ;; by following the spec
     (if-let [read-fn (if (fn? spec)
                        spec
                        (:struct/read (meta spec)))]
       (read-fn bb context)
       (read-spec spec bb (or data {}) context)))))

(defn- rw-fns
  "Given a user supplied context object, try to fetch the :rw-fns field.
  Optionally, fetch the vector that represents reading/writing a specific type of field.

  The user may have supplied an atom if they needed to track some mutable data through
  the serialization process. If that is the case, then fetching the :rw-fns field
  becomes just ever so slightly more complicated than simply looking up a key."
  ([context]
   (rw-fns context nil))

  ([context spec]
   (cond-> context
     (atom? context) (deref)
     true (:rw-fns)
     (some? spec) (get spec)))

  ([context spec read-or-write]
   (let [fns (rw-fns context spec)]
     (cond
       (= :read read-or-write) (nth fns 2)
       (= :write read-or-write) (nth fns 3)))))

(defmethod read-spec :default
  [spec bb data context]

  (cond

    ;; If the spec has been compiled, we will get a function in the place of a
    ;; keyword indicating a primitive.
    ;; Call it with the byte buffer to read it
    (fn? spec)
    (spec bb context)

    (struct-def? spec)
    (reduce (fn [data [field-name field-spec]]
              (when *debug*
                (println (format "%-20s %s %2s %s" "reading field:" field-name "" field-spec)))

              ;; If the spec looks like it's asking to attach some static data
              ;; to the map...
              (if (= (namespace field-name) "static")
                ;; Attach the static data. We're done with this iteration
                (assoc data field-name field-spec)

                ;; Otherwise, try to read the spec and attach the result
                (let [field-val (read-spec field-spec bb data context)]
                  (when *debug*
                    (print (format "%20s" "") field-name "=> ")
                    (clojure.pprint/pprint field-val))

                  ;; Keep reading and looping until we're done with the spec pairs
                  (assoc data field-name field-val))))
            data
            (partition 2 spec))


    ;; Otherwise, we should be dealing with some kind of primitive...
    ;; spec, we should be dealing with some kind of primitive...
    ;; Look up the primitive keyword in the primitives map
    :else
    (if-let [read-fn (rw-fns context spec :read)]

      ;; Execute the read function now
      (read-fn bb context)

      ;; If a read function cannot be found, give up...
      (do
        (when *debug*
          (println "context:" context)
          (println "spec:" spec)
          (println "fn:" (rw-fns context spec)))
        (throw (ex-info (str "Cannot handle spec:" spec) {}))))))


(defn struct-def
  "Tags the given spec so read-structure will return a map instead of a vector.
  If the item in the list is a map, it will used as the meta info of the structure.
  Returns the collection it was given."
  [& fields]
  (let [[fields meta-info] (if (map? (last fields))
                             [(butlast fields) (last fields)]
                             [fields {}])
        meta-info (merge meta-info {:struct/return-type :map})]
    (with-meta (into [] fields) meta-info)))


(defn array
  "Tags the given spec to say it will repeat a number of times.

  Exmaple:
  (array :uint32 :count-prefix :uint16)

  This says we will read a :uint16 first to determine how many :uint32 to read
  from the stream."
  [spec & {:keys [length-prefix length read-length-fn skip-write-length]
           :or {length-prefix :int32
                length -1}}]

  ;; We're about to attach some meta info to the spec
  ;; First, wrap the spec in another collection...
  ;; We'll attach the information to information to that collection instead of the
  ;; original spec
  (let [spec-seq (list spec)]

    ;; Attach the meta info to the spec
    ;; Any sequence can be have meta info attached
    (with-meta spec-seq
      {:struct/type :array
       :struct/length length
       :struct/length-prefix length-prefix
       :struct/read-length-fn read-length-fn
       :struct/skip-write-length skip-write-length})))


(defmethod read-spec :array
  [spec bb data context]

  (let [;; Destructure fields in the attached meta info
        {static-length :struct/length
         length-prefix :struct/length-prefix
         length-fn :struct/read-length-fn} (meta spec)

        ;; Read out the count of the spec to read
        length (cond
                 (not= static-length -1) static-length
                 (fn? length-fn) (length-fn bb data context)
                 :else (read-spec length-prefix bb data context))

        _ (when *debug*
            (println "array:" length))]

    (into [] (for [_ (range length)]
               (read-struct (first spec) bb context)))))

(defn string
  "Returns a spec used to read a string of a specific type of encoding.
  Can supply :length-prefix to indicate how to read the length of the string.
  The default :length-prefix is a :int32."
  [enc & {:keys [length-prefix length]
          :or {length-prefix :int32
               length -1}}]

  ;; Tag a dummy sequence with the info passed into the function
  (with-meta '(:string)
    {:struct/type :string
     :struct/length length
     :struct/length-prefix length-prefix
     :struct/string-encoding enc}))


(defn- buffer-size-for-string
  [length encoding]

  (cond
    (= encoding :utf-16-le)
    (* length 2)

    :else
    length))


(defmethod read-spec :string
  [spec ^ByteBuffer bb data context]

  (let [valid-encodings {:ascii "US-ASCII"
                         :utf-8 "UTF-8"
                         :utf-16-le "UTF-16LE"}

        ;; Destructure fields in the attached meta info
        {static-length :struct/length
         length-prefix :struct/length-prefix
         requested-encoding :struct/string-encoding} (meta spec)

        ;; What is the length of the string we want to read?
        ;; If a static length has been configured, use it
        ;; Otherwise, read the length
        length (if (not= static-length -1)
                 static-length
                 (read-spec length-prefix bb data context))

        ;; Create a temp buffer to hold the bytes before turning it into a java string
        buffer (byte-array (buffer-size-for-string length requested-encoding))]

    ;; Read the string bytes into the buffer
    (io.core/get-byte-array bb buffer 0 (buffer-size-for-string length requested-encoding))

    ;; If the context asked for bytes to be transformed, do it now
    ;; FIXME!!!  Reading API for byte array is very different here.
    ;;           Would be nice if this is "read-bytes".
    (if-let [transform-fn (rw-fns context :transform-bytes!)]
      (transform-fn buffer context))

    (if (= :bytes requested-encoding)
      buffer
      (String. ^bytes buffer ^String (valid-encodings requested-encoding)))))


(defn seq->bytes
  [seq]

  ;; Construct a byte array from the contents of...
  (byte-array

   ;; sequence of bytes reduced from individual items in the given sequence
   (reduce
    (fn [accum item]
      (conj accum (byte item))) [] seq)))


(defn bytes->bytebuffer
  [bytes]
  (ByteBuffer/wrap bytes))


(defn seq->bytebuffer
  [seq]
  (-> seq
      seq->bytes
      bytes->bytebuffer))


(defn- prim-spec-get-write-fn
  [prim-specs type]
  (nth (prim-specs type) 3))

(declare write-struct)

(defmulti write-spec spec-type)

(defmethod write-spec :array
  [spec ^ByteBuffer bb data context]

  (let [;; Destructure fields in the attached meta info
        {static-length :struct/length
         length-prefix :struct/length-prefix
         skip-write-length :struct/skip-write-length} (meta spec)]

    ;; Write out the spec if we're not dealing with a sequence with a static/implied length
    (when (and (= static-length -1)
               (not skip-write-length))

        ;; Write out the length of the sequence first
        (let [write-fn (prim-spec-get-write-fn (rw-fns context) length-prefix)]
          (write-fn bb (count data) context)))

    (when *debug*
      (println "[write-spec :array] spec" spec))

    ;; Write out each item in the sequence
    (doseq [item data]
      (write-struct (first spec) bb item context))))

(defmethod write-spec :string
  [spec ^ByteBuffer bb data context]

  (assert (not (nil? data)))

  (let [valid-encodings {:ascii "US-ASCII"
                         :utf-8 "UTF-8"
                         :utf-16-le "UTF-16LE"}

        ;; Destructure fields in the attached meta info
        {static-length :struct/length
         length-prefix :struct/length-prefix
         requested-encoding :struct/string-encoding} (meta spec)


        ;; Grab the string's byte representation
        str-bytes (if (= requested-encoding :bytes)

                    ;; If we're just looking at some raw bytes, we don't have to do anything...
                    (bytes data)

                    ;; Otherwise, we're looking at some kind of string
                    ;; Get the raw bytes after converting the string to the right encoding
                    (->> (requested-encoding valid-encodings)
                         (java.nio.charset.Charset/forName)
                         (.getBytes data)))

        ;; Write out the length of the string itself, unless it is of a static length,
        ;; in which case, we'll say the length is implicit.
        _ (when (= static-length -1)
            ;; What is the string length we're writing out to file?
            (let [claimed-str-length (count data)
                  write-fn (prim-spec-get-write-fn (rw-fns context) length-prefix)]
              (write-fn bb claimed-str-length context)))

        ;; If the context asked for bytes to be transformed, do it now
        str-bytes (if-let [transform-fn (rw-fns context :transform-bytes!)]
                    (transform-fn str-bytes context)
                    str-bytes)]

    ;; The string has now been converted to the right encoding and transformed.
    ;; Write it out now.
    (.put bb str-bytes)))

(defmethod write-spec :default
  [spec ^ByteBuffer bb data context]

  ;; Look up how we're supposed to deal with this primitive
  ;; If the primitive is found...
  (if-let [write-fn (rw-fns context spec :write)]

      ;; Execute the read function now
      (write-fn bb data context)

      ;; Otherwise, give up
      (throw (Throwable. (str "Cannot handle spec:" spec)))))

(defn write-struct
  [spec ^ByteBuffer bb data context]
  {:pre [(not (nil? data))]}

  (when *debug*
    (println "[write-struct] spec:" spec)
    (println "[write-struct] (meta spec):" (meta spec)))

  (swap! context assoc :mode :write)

  (cond
    ;; Did the spec attach a write function?
    ;; If so, call it now
    (some? (:struct/write (meta spec)))
    ((:struct/write (meta spec)) bb data context)

    ;; If the spec looks like it is describing fields of a map...
    (struct-def? spec)
    (do
      ;; Did the spec say to use this piece of data as an anchor?
      ;; If so, push this piece of data onto a stack in the context
      (when (:anchor (meta spec))
        (swap! context update :anchor-stack conj data))


      (doseq [[key type] (partition 2 spec)
              :let [val (data key)]]
        (when-not (= (namespace key) "static")
          (write-spec type bb val context)))

      ;; Did the spec say to use this piece of data as an anchor?
      ;; If so, pop the data off of the stack
      (when (:anchor (meta spec))
        (swap! context update :anchor-stack pop)))

    ;; Otherwise, let our multi-method's default handling do it's thing
    :else
    (write-spec spec bb data context)))

;;------------------------------------------------------------------------------
;; Conditional rule handling
;;------------------------------------------------------------------------------
(defn conditional
  "Tags a function to be executed during reading/writing.
  The function will receive a single parameter, which will the be data that has been
  read so far.

  The idea is to be able to conditionally enable or change the spec attached to a field.
  For example, this is useful when a file is versioned and certain fields optionally appear
  or change into something else based on the version of the file being processed."
  [fn]

  (with-meta fn (merge (meta fn) {:struct/type :conditional})))


(defmethod read-spec :conditional
  [cond-fn bb data context]

  (assert (fn? cond-fn))

  ;; Send the currently read data to the cond-fn to retrieve the spec to be used
  (when-let [spec (cond-fn data context)]
    (when *debug*
      (println "conditional => " spec))
    ;; Read the data using the retrieved spec if any
    (read-spec spec bb data context)))


(defmethod write-spec :conditional
  [cond-fn bb data context]

  (assert (fn? cond-fn))

  ;; Send the currently read data to the cond-fn to retrieve the spec to be used
  (when-let [spec (cond-fn data context)]
    ;; write the data using the retrieved spec if any
    (write-spec spec bb data context)))
