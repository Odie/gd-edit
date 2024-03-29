(ns gd-edit.io.arz
  (:require [gd-edit.structure :as s]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [taoensso.timbre :as log]
            [gd-edit.utils :as u]
            ;; [clj-async-profiler.core :as prof]
            )
  (:import  [java.nio ByteBuffer]
            [net.jpountz.lz4 LZ4Factory])
  (:gen-class))

(def arz-header
  (s/struct-def
   :unknown              :int16
   :version              :int16
   :record-table-start   :int32
   :record-table-size    :int32
   :record-table-entries :int32
   :string-table-start   :int32
   :string-table-size    :int32))


(def arz-string-table
  (s/array
   (s/string :ascii)
   :length-prefix :int32))

(defn- load-db-header
  [^ByteBuffer bb]

  ;; Jump to the start of the file header and start decoding
  (.position bb 0)
  (s/read-struct arz-header bb))


;; After some number of attempts to get gloss to load the string table, I gave up and just
;; hand-coded the string table reading.
;; (defn- load-db-string-table2
;;   [^ByteBuffer bb header]

;;   ;; Jump to the start of the string table and start decoding
;;   (.position bb (:string-table-start header))
;;   (read-struct (compile-spec arz-string-table) bb))


(defn- load-db-string-table
  [^ByteBuffer bb header]

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
        (let [str-len (.getInt bb)
              str-buffer (byte-array str-len)]

          ;; Read the specified number of bytes and convert it to a string
          (.get bb str-buffer 0 str-len)

          ;; Either continue on to the next loop or
          ;; terminate by returning the string table
          (recur (inc i) limit (conj string-table (String. str-buffer))))))))


(def arz-record-header
  (s/struct-def
   :filename          :int32
   :type              (s/string :ascii)
   :offset            :int32
   :compressed-size   :int32
   :decompressed-size :int32
   :unknown           :int32
   :unknown2          :int32
   ))


(defn- load-db-records-header-table
  [^ByteBuffer bb header string-table]

  ;; Move the buffer to the beginning of the header
  (.position bb (:record-table-start header))

  ;; Read all the header entries
  (->> (for [_ (range (:record-table-entries header))]
         (s/read-struct arz-record-header bb))

       ;; Look up all the record file names in the string table
       (map (fn [item]
              (->> (item :filename)
                   (nth string-table)
                   (assoc item :filename))))))


(defn- load-db-record
  [^ByteBuffer bb header string-table record-header localization-table]

  (let [{:keys [compressed-size decompressed-size]} record-header
        compressed-data (byte-array (:compressed-size record-header))
        decompressed-data (byte-array (:decompressed-size record-header))
        decompressor (.fastDecompressor (LZ4Factory/fastestInstance))

        ;; Move to where the record is
        ;; Note: we're adding 24 to account for the file header
        _ (.position bb (+ (:offset record-header) 24))

        ;; Grab the compressed data
        _ (.get bb compressed-data 0 compressed-size)

        ;; Decompress the data
        _ (.decompress decompressor compressed-data 0 decompressed-data 0 decompressed-size)

        record-buffer (ByteBuffer/wrap (bytes decompressed-data))
        _ (.order record-buffer java.nio.ByteOrder/LITTLE_ENDIAN)
        ]

    ;; Try to read the entire record...
    (reduce
     (fn [record _]
       ;; Did we finish reading the entire record?
       (if (<= (.remaining record-buffer) 0)
         ;; If so, return the accumulated record
         (reduced (persistent! record))

         ;; Otherwise, read one more entry...
         (let [type (.getShort record-buffer)

               data-count (.getShort record-buffer)
               fieldname (nth string-table (.getInt record-buffer))

               get-one-field (fn []
                               (cond (= type 1)
                                     (.getFloat record-buffer)

                                     ;; Looking at a string?
                                     (= type 2)

                                     ;; Look up the string from the string table
                                     (let [str (nth string-table (.getInt record-buffer))]

                                       ;; If the string looks like a "tag" name used for localization...
                                       (if (str/starts-with? (str/lower-case str)"tag")
                                         ;; (do
                                         ;;   (if (nil? (localization-table str))
                                         ;;     ;; (u/print-line "replacing \"" str "\" with" (localization-table str))
                                         ;;     (u/print-line "could not replace localization string:" str)
                                         ;;     ))

                                         ;; Look it up in the localization table
                                         (or (localization-table str) str)

                                         ;; Otherwise, just use the string as is
                                         str
                                         )
                                       )

                                     :else
                                     (.getInt record-buffer)))

               ;; Read out the indicate number of data items into a list
               val (if (= data-count 1)

                     ;; Only one item to get?
                     ;; Read the value and be done
                     (get-one-field)

                     ;; We have more than one value?
                     ;; Setup and loop and read out all the values into a vector
                     (loop [i 0
                            limit data-count
                            accum (transient [])]
                       ;; Did we retrieve all the items?
                       (if (>= i limit)

                         ;; If so, return the items now
                         (persistent! accum)

                         ;; Otherwise, grab one more item and loop
                         (recur (inc i)
                                limit
                                (conj! accum (get-one-field))))))
               ]

           (cond
             ;; Do we have more than one value?
             ;; Add it to the record
             (> 1 data-count)
             (assoc! record fieldname val)

             ;; Otherwise, we only have a single value in the vector
             ;; If the value is not zero or empty string, add it to the record
             (and (not= val 0)
                  (not= val (float 0))
                  (not= val ""))
             (assoc! record fieldname val)

             ;; Otherwise, don't append any new fields to the record
             ;; Just return it as is
             :else
             record
             ))))
     (transient {}) ;; Start reduce with an empty record
     (repeat 0)     ;; Have reduce loop forever. We'll check the exit condition in the lambda
     )))


(defn- load-db-records
  [^ByteBuffer bb header string-table localization-table]

  ;; Load up all the record headers
  (->> (load-db-records-header-table bb header string-table)

       ;; Try to read each record
       (pmap (fn [record-header]

               (let [bb (.duplicate bb)]

                 ;; Read a single record
                 (-> (load-db-record bb header string-table record-header localization-table)

                     ;; Add in the recordname from the header
                     (assoc :recordname (:filename record-header))))))

       (doall)))

(defn load-game-db
  [filepath localization-table]

  (log/debug "Entering load-game-db")
  (u/log-exp filepath)

  ;; Open the database file
  (let [bb (u/mmap filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        ;; Read and parse the header
        header (load-db-header bb)
        string-table (load-db-string-table bb header)]

    ;; Read in all the file records
    (load-db-records bb header string-table localization-table)))

(defn dump-db-records
  [db-records outpath]

  ;; For every record in the db-records collection
  ;; We do this in parallel because there is a lot of records to write...
  (doall
   (pmap (fn [record]

          ;; If we can retrieve a valid recordname/filename
          (let [recordname (:recordname record)]
            (when-not (empty? recordname)

              ;; Grab the file contents...
              (let [contents (->> record
                                  (filter #(not (keyword? (key %))))
                                  (into {}))
                    record-path (io/file outpath recordname)]

                ;; Write the contents to disk
                (.mkdirs (.getParentFile record-path))
                ;; Print contents directly to file as edn
                ;; This is very fast, but not pretty-printed
                ;; (spit record-path (pr-str contents))

                ;; Prints the content using the default clojure pretty-printer
                ;; Too slow to use on this dataset
                ;; (with-open [writer (clojure.java.io/writer record-path)]
                ;;   (pp/pprint contents writer))

                (spit record-path (with-out-str (pp/pprint contents)))
                ))))

        db-records))
  nil)


(comment

  (time (def db
          (load-game-db
           (first (gd-edit.game-dirs/get-db-file-overrides))
           @gd-edit.globals/localization-table)))

  (do
    (prof/start {:interval (/ 1000000 2)})
    (def db (load-game-db
             (first (gd-edit.game-dirs/get-db-file-overrides))
             @gd-edit.globals/localization-table))

    (prof/stop {}))

  (prof/serve-files 9000)

  ;; Make sure all loaded items are persistent collections
  (def t
    (u/collect-walk-entire-tree #(and (instance? clojure.lang.Seqable %)
                                          (not (instance? clojure.lang.IPersistentCollection %))) db))

  ;; Dump each db record into a directory as individual files
  (time (dump-db-records @gd-edit.globals/db (io/file (gd-edit.utils/working-directory) "database")))
  )
