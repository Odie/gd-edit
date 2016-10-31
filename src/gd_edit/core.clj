(ns gd-edit.core
  (:require [clojure.java.io :as io]
            [gloss.core :as gloss]
            [gloss.io])
  (:import  [java.nio ByteBuffer]
            [java.nio.file Path Paths Files FileSystems StandardOpenOption]
            [java.nio.channels FileChannel])
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def primitives-specs
  ;; name => primitives spec
  {:byte   [:byte    1 #(.get %)      ]
   :int16  [:int16   2 #(.getShort %) ]
   :int32  [:int     4 #(.getInt %)   ]
   :float  [:float   4 #(.getFloat %) ]
   :double [:double  4 #(.getDouble %)]
   })

(defn sizeof
  "Given a spec, return the byte size it represents"
  [spec]

  ;; Look through all the items in the given spec
  (reduce
   (fn [accum item]
     ;; Does a matching entry exist for the item in the primitives array?
     (let [primitives-spec (primitives-specs item)]
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

(defn read-struct
  [spec ^ByteBuffer bb]

  (reduce
   (fn [accum item]
     ;; Does a matching entry exist for the item in the primitives array?
     (let [primitives-spec (primitives-specs item)]
       (if primitives-spec
         ;; Look up the associated function to retrieve the item from the byte buffer
         ;; Run it with the supplied byte buffer and add that to the results
         (conj accum ((nth primitives-spec 2) bb))

         ;; If not, we've come across a field name. Place the fieldname into the result.
         (conj accum item)
         )))
   [] spec))


(def arz-header
  [:unknown              :int16
   :version              :int16
   :record-table-start   :int32
   :record-table-size    :int32
   :record-table-entries :int32
   :string-table-start   :int32
   :string-table-size    :int32])

;; (gloss/defcodec arz-record-header-codec
;;   (gloss/ordered-map
;;    :type :uint16-le
;;    :count :uint16-le
;;    :fieldname-index   :uint32-le))

;; (gloss/defcodec arz-string-table-codec
;;   (gloss/repeated
;;    (gloss/finite-frame
;;     (gloss/prefix :uint32-le)
;;     (gloss/string :ascii))
;;    :prefix :uint32-le))


(defn mmap
  [filepath]

  (with-open [db-file (java.io.RandomAccessFile. filepath "r")]
    (let [file-channel (.getChannel db-file)
          file-size (.size file-channel)]

          (.map file-channel java.nio.channels.FileChannel$MapMode/READ_ONLY 0 file-size))))


(defn- load-db-header
  [^java.nio.ByteBuffer bb]
  
  ;; Jump to the start of the file header and start decoding
  (.position bb 0)
  (read-struct arz-header bb))


;; After some number of attempts to get gloss to load the string table, I gave up and just
;; hand-coded the string table reading.
(defn- load-db-string-table
  [^java.nio.ByteBuffer bb header]

  ;; Jump to the start of the string table and start decoding
  (.position bb (:string-table-start header))

  ;; How many strings do we have?
  (let [str-count (.getInt bb)]

    ;; Read out that many string
    (loop [i 0
           limit str-count
           string-table []]

      ;; Did we retrieve the targetted number of strings?
      ;; If so, just return the collected string table
      (if (>= i limit)
        string-table

        ;; Retrieve one more string
        (do
           (let [str-len (.getInt bb)
                 str-buffer (byte-array str-len)]


             ;; Read the specified number of bytes and convert it to a string
             (.get bb str-buffer 0 str-len)

             ;; Either continue on to the next loop or
             ;; terminate by returning the string table
             (recur (inc i) limit (conj string-table (String. str-buffer)))))))))


(defn load-game-db
  [filepath]

  ;; Open the database file
  (let [bb (mmap filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        ;; Read and parse the header
        header (load-db-header bb)
        string-table (load-db-string-table bb header)

        ;; Read in all the file records
        ]

        ;; Read in the individual file parts

    (last string-table)))


#_(load-game-db "/Users/Odie/Dropbox/Public/GrimDawn/database/database.arz")


#_(def f (mmap "/Users/Odie/Dropbox/Public/GrimDawn/database/database.arz"))
#_(.order f java.nio.ByteOrder/LITTLE_ENDIAN)
#_(def h (load-db-header f))
#_(.position f (:string-table-start h))
#_(gloss.io/decode arz-string-table-codec f false)
#_(gloss.io/encode arz-string-table-codec ["hello" "world"])
