(ns gd-edit.gdc-reader
  (:require [gd-edit.structure :as s]
            [gd-edit.utils :as utils])
  (:import  [java.nio ByteBuffer]))


(def FilePreamble
  (s/ordered-map
   :magic   :int32
   :version :int32))

(def Header
  (s/ordered-map
   :character-name    (s/string :utf-16-le)
   :male              :bool
   :player-class-name (s/string :ascii)
   :character-level   :int32
   :hardcore-mode     :bool))

(def Block1
  (s/ordered-map
   :version              :int16
   :in-main-quest        :bool
   :has-been-in-game     :bool
   :last-difficulty      :byte
   :greatest-difficulty-completed :byte
   :iron                 :int32
   :greatest-survival-difficulty-completed :byte
   :tributes             :int32
   :ui-compass-state     :byte
   :always-show-loot     :uint32
   :show-skill-help      :bool
   :alt-weapon-set       :bool
   :alt-weapon-set-enabled :bool
   :player-texture       :string))



(defn unsigned-long
  [val]

  (if (= (type val) java.lang.Integer)
    (.toUnsignedLong val)
    val))

(def byte-array-type (Class/forName "[B"))

(defn enc-next-state-with-byte
  "Given a byte array, the current encryption state, and an encryption table, return the next enc state"
  [byte-data enc-state enc-table]

  (bit-xor enc-state (enc-table (Byte/toUnsignedInt byte-data))))

(defn enc-next-state-with-byte-array
  "Given a single byte, the current encryption state, and an encryption table, return the next enc state"
  [byte-data enc-state enc-table]

  (loop [i 0
         limit (count byte-data)
         state enc-state]

    ;; Processed all the byte data?
    (if (>= i limit)
      state ; return the new encryption state

      ;; Update the encryption state one more time
      (recur (inc i)
             limit
             (enc-next-state-with-byte (nth byte-data i) state enc-table)))))

(defn enc-next-state
  [byte-data enc-state enc-table]

  ;; Dispatch by byte-data type
  (if (sequential? byte-data)

    (enc-next-state-with-byte-array byte-data enc-state enc-table)

    (enc-next-state-with-byte byte-data enc-state enc-table)))

(defn decrypt-bytes!
  [buffer context]
  (loop [i 0
         limit (count buffer)
         enc-state (:enc-state context)
         enc-table (:enc-table context)
         ]

    ;; Finished transforming the byte array?
    (if (>= i limit)
        ;; Return the new enc-state
        enc-state


      ;; Not done?
      ;; Grab the next byte, decrypt it, store it back in the same spot
      (let [enc-val (aget buffer i)
            val (byte (bit-and 0x00000000000000ff (bit-xor enc-val enc-state)))]

        (aset buffer i val)
        (recur (inc i) limit (enc-next-state enc-val enc-state enc-table) enc-table)
        ))))

(defn transform-bytes!
  "Decrypts the given byte array in place while updating the encryption context"
  [buffer context-atom]

  (let [next-enc-state (decrypt-bytes! buffer @context-atom)]

    ;; Update the context with the new state
    (reset! context-atom (assoc  @context-atom :enc-state next-enc-state))))

(defn- read-bytes-
  [^ByteBuffer bb {:keys [enc-state enc-table] :as context} byte-count]

  (let [buffer (byte-array byte-count)]
    (.get bb buffer 0 byte-count)

    [buffer (decrypt-bytes! buffer context)]))

(defn- read-byte-
  "Retrieve the next byte, decrypt the value, then return the value and the next enc state"
  [^ByteBuffer bb {:keys [enc-state enc-table] :as context}]

  (let [enc-val (.get bb)]
    [(byte (bit-and 0x00000000000000ff (bit-xor enc-val enc-state))) (enc-next-state enc-val enc-state enc-table)]))

(defn- read-int-
  "Retrieve the next byte, decrypt the value, then return the value and the next enc state"
  [^ByteBuffer bb {:keys [enc-state enc-table] :as context}]

  (let [enc-val (.getInt bb)
        byte-data (-> (ByteBuffer/allocate 4)
                      (.putInt enc-val)
                      (.array)
                      (reverse)
                      )]

    ;; Return [decrypted-value next-enc-state] pair
    [(bit-and 0x00000000ffffffff (bit-xor enc-val enc-state)) (enc-next-state byte-data enc-state enc-table)]))

(defn- read-bool-
  [^ByteBuffer bb {:keys [enc-state enc-table] :as context}]

  (let [[val next-enc-state] (read-byte bb context)]
    [(= val 1) next-enc-state]))

(defn- read-float-
  "Retrieve the next byte, decrypt the value, then return the value and the next enc state"
  [^ByteBuffer bb {:keys [enc-state enc-table] :as context}]

  (let [val (bit-xor (.getFloat bb) enc-state)
        byte-data (-> (ByteBuffer/allocate 4)
                      (.putFloat val)
                      (.array))]

    [val (enc-next-state byte-data enc-state enc-table)]))

(def read-bytes (partial read-and-update-context read-bytes-))
(def read-bool (partial read-and-update-context read-bool-))
(def read-int (partial read-and-update-context read-int-))
(def read-float (partial read-and-update-context read-float-))

(defn read-and-update-context
  [read-fn ^ByteBuffer bb context & rest]

  ;; Run the read function
  (let [[val new-enc-state] (apply read-fn bb @context rest)]

    ;; Update the enc-state
    (reset! context (assoc @context :enc-state new-enc-state))

    ;; Return the read value
    val))


(defn make-enc-context
  [enc-state enc-table]

  (atom {;; Mutable state
         :enc-state enc-state
         :enc-table enc-table

         ;; Specs table for the structure lib
         :primitive-specs {:transform-bytes! transform-bytes!
                           :byte   [:byte    1 read-byte ]
                           :bool   [:byte    1 read-bool ]
                           :int32  [:int32   4 read-int  ]
                           :float  [:float   4 read-float]}
         }))

(defn generate-encryption-table
  "Return a sequence of 256 elements, each being an 4 byte quantity, derived from the given seed"
  [seed]

  (loop [i 0
         limit 256
         val seed
         table (transient [])
         ]

    ;; Did we finish constructing the table?
    (if (>= i limit)
      (persistent! table)

      ;; Generate one more value for the table
      ;;
      ;; Song and dance for the jvm.
      ;; We just want to manipulate some bit patterns here but jvm doesn't make it very easy because it insists not
      ;; acknowledging unsigned numbers.
      ;; Basically, we want to do
      ;;   uint new-val = (val << 31 | val >> 1) * 39916801
      (let [new-val (Integer/toUnsignedLong
                     (.intValue
                      (unchecked-multiply
                       (long (bit-or (bit-shift-left val 31) (bit-shift-right val 1)))
                       (long 39916801))))]

        (recur (inc i)
               limit
               new-val
               (conj! table new-val))))))


(defn validate-preamble
  [preamble]

  (if (or (not= (:magic preamble) 0x58434447)
          (not= (:version preamble) 1))
    (throw (Throwable. "I don't understand this gdc format!"))
    ))

(defn load-character-file
  [filepath]

  (let [bb (utils/mmap filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        seed (bit-xor (.getInt bb) 1431655765)
        enc-table (generate-encryption-table seed)
        enc-context (make-enc-context seed enc-table)

        preamble (s/read-struct FilePreamble bb enc-context)
        _ (validate-preamble preamble)


        header (s/read-struct Header bb enc-context)

        header-checksum (Integer/toUnsignedLong (.getInt bb))
        _ (assert (= header-checksum (:enc-state @enc-context)))

        data-version (read-int bb enc-context)
        _ (if (not (contains? #{6 7} data-version))
            (throw (Throwable. "I can't read this gdc format!")))

        mystery-field (read-bytes bb enc-context 16)]

    mystery-field))

#_(def r (load-character-file "/Users/Odie/Dropbox/Public/GrimDawn/main/_Hetzer/player.gdc"))
