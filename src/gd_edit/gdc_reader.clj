(ns gd-edit.gdc-reader
  (:require [gd-edit.structure :as s]
            [gd-edit.utils :as utils])
  (:import  [java.nio ByteBuffer]))


(declare read-block read-byte! read-int! read-bool! read-float!)

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
   :version                :int32
   :in-main-quest          :bool
   :has-been-in-game       :bool
   :last-difficulty        :byte
   :greatest-difficulty-completed :byte
   :iron                   :int32
   :greatest-survival-difficulty-completed :byte
   :tributes               :int32
   :ui-compass-state       :byte
   :always-show-loot       :int32
   :show-skill-help        :bool
   :alt-weapon-set         :bool
   :alt-weapon-set-enabled :bool
   :player-texture         (s/string :ascii)))

(def Block2
  (s/ordered-map
   :version                :int32
   :level-in-bio           :int32
   :experience             :int32
   :modifier-points        :int32
   :skill-points           :int32
   :devotion-points        :int32
   :total-devotion-points-unlocked :int32
   :physique               :float
   :cunning                :float
   :spirit                 :float
   :health                 :float
   :energy                 :float
   ))


(def Item
  (s/ordered-map
   :basename       (s/string :ascii)
   :prefix-name    (s/string :ascii)
   :suffix-name    (s/string :ascii)
   :modifier-name  (s/string :ascii)
   :transmute-name (s/string :ascii)
   :seed           :int32

   :relic-name     (s/string :ascii)
   :relic-bonus    (s/string :ascii)
   :relic-seed     :int32

   :augment-name   (s/string :ascii)
   :unknown        :int32
   :augment-seed   :int32

   :var1           :int32
   :stack-count    :int32))

(def InventoryItem
  (into Item
        (s/ordered-map
         :X :float
         :Y :float)))

(def StashItem
  (into Item
        (s/ordered-map
         :X :float
         :Y :float)))

(def EquipmentItem
  (into Item
        (s/ordered-map
         :attached :bool)))

(defn merge-meta
  "Merges the provided map into the meta map of the provided object."
  [obj map]
  (with-meta obj (merge (meta obj) map)))

(def InventorySack
  (s/ordered-map
   :unused :bool
   :invetory-items (s/variable-count InventoryItem)))

(def Block0 InventorySack)

;; For reference only
(def Block3
  (s/ordered-map
   :version           :int32
   :has-data          :bool
   :sack-count        :int32
   :focused-sack      :int32
   :selected-sack     :int32
   :inventory-sacks   :ignore
   :use-alt-weaponset :bool
   :equipment         :ignore
   :alternate1        :bool
   :alternate1-set    :ignore
   :alternate2        :bool
   :alternate2-set    :ignore))


(defn read-block3
  [^ByteBuffer bb context]

  #break
  (let [version (read-int! bb context)
        has-data (read-bool! bb context)
        sack-count (read-int! bb context)
        focused-sack (read-int! bb context)
        selected-sack (read-int! bb context)

        inventory-sacks (reduce (fn  [accum _]
                                  (conj accum (read-block bb context)))
                         []
                         (range sack-count))

        use-alt-weaponset (read-bool! bb context)

        equipment (reduce (fn  [accum _]
                            (conj accum (s/read-struct EquipmentItem bb context)))
                          []
                          (range 12))

        alternate1 (read-bool! bb context)
        alternate1-set (reduce (fn  [accum _]
                            (conj accum (s/read-struct EquipmentItem bb context)))
                          []
                          (range 2))

        alternate2 (read-bool! bb context)
        alternate2-set (reduce (fn  [accum _]
                                 (conj accum (s/read-struct EquipmentItem bb context)))
                               []
                               (range 2))
        ]
    {:version           version
     :has-data          has-data
     :sack-count        sack-count
     :focused-sack      focused-sack
     :selected-sack     selected-sack
     :inventory-sacks   inventory-sacks
     :use-alt-weaponset use-alt-weaponset
     :equipment         equipment
     :alternate1        alternate1
     :alternate1-set    alternate1-set
     :alternate2        alternate2
     :alternate2-set    alternate2-set
     }
    ))

(def Block4
  (s/ordered-map
    :version :int32
    :stash-width :int32
    :stash-height :int32
    :stash-items (s/variable-count StashItem)))

(def UID
  (s/string :ascii :length 16))


(def Block5
  (s/ordered-map

   :version :int32

   :spawn-points (s/variable-count
                  (s/variable-count UID)
                  :length 3)

   ;; [StaticCount(3)]
   ;; public List<UID> currentRespawn = new List<UID>();

   :current-respawn (s/variable-count UID
                     :length 3)
))

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
            val (.byteValue (bit-and 0x00000000000000ff (bit-xor enc-val enc-state)))]

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
    [(short (bit-and 0x00000000000000ff (bit-xor enc-val enc-state))) (enc-next-state enc-val enc-state enc-table)]))

(defn decrypt-int
  [val enc-state]

  ;; We want to xor the value with the enc-state to get back the decrypted int.
  ;; However, since enc-state is stored as a long, we'll want to do some additional
  ;; masking to get rid of the unwanted bits.
  ;; Otherwise, when we try to turn the long value back into an int, jvm will throw
  ;; an "out of range" exception.

  ;; decrypt the value by xor-ing
  (-> (bit-xor val enc-state)

      ;; mask out useless bits (from the enc-state)
      (bit-and 0x00000000ffffffff)

      ;; turn the value back into an int
      (int)))

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
    [(decrypt-int enc-val enc-state) (enc-next-state byte-data enc-state enc-table)]))

(defn- read-bool-
  [^ByteBuffer bb {:keys [enc-state enc-table] :as context}]

  (let [[val next-enc-state] (read-byte- bb context)]
    [(= val 1) next-enc-state]))

(defn- read-float-
  "Retrieve the next byte, decrypt the value, then return the value and the next enc state"
  [^ByteBuffer bb {:keys [enc-state enc-table] :as context}]

  ;; Java doesn't allow bit-wise manipulation for floats
  ;; We'll have to read a 4 byte quantity, decrypt it, then reinterpret it as a float
  (let [enc-val (.getInt bb)
        byte-data (-> (ByteBuffer/allocate 4)
                      (.putInt enc-val)
                      (.array)
                      (reverse))
        reinterpreter (ByteBuffer/allocate 4)
        ]

    [(-> reinterpreter
         ;; Decrypt the bit pattern
         ;; And re-interpret the bits as a float
         (.putInt (bit-and 0x00000000ffffffff (bit-xor enc-val enc-state)))
         (.flip)
         (.getFloat))

     (enc-next-state byte-data enc-state enc-table)]))


(defn read-and-update-context
  [read-fn ^ByteBuffer bb context & rest]

  ;; Run the read function
  (let [[val new-enc-state] (apply read-fn bb @context rest)]

    ;; Update the enc-state
    (reset! context (assoc @context :enc-state new-enc-state))

    ;; Return the read value
    val))

(def read-bytes! (partial read-and-update-context read-bytes-))
(def read-byte!  (partial read-and-update-context read-byte-))
(def read-bool!  (partial read-and-update-context read-bool-))
(def read-int!   (partial read-and-update-context read-int-))
(def read-float! (partial read-and-update-context read-float-))

(defn make-enc-context
  [enc-state enc-table]

  (atom {;; Mutable state
         :enc-state enc-state
         :enc-table enc-table

         ;; Specs table for the structure lib
         :primitive-specs {:transform-bytes! transform-bytes!
                           :byte   [:byte    1 read-byte! ]
                           :bool   [:byte    1 read-bool! ]
                           :int32  [:int32   4 read-int!  ]
                           :float  [:float   4 read-float!]}
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
    (throw (Throwable. "I don't understand this gdc format!"))))



(defn read-block
  [^ByteBuffer bb context]

  (let [;; Read the block id
        id (read-int! bb context)

        ;; Try to fetch the block spec by name
        block-spec-var (resolve (symbol (str "Block" id)))
        block-spec (if-not (nil? block-spec-var)
                     (var-get block-spec-var)
                     nil)

        ;; Try to fetch a custom read function by name
        block-read-fn-var (resolve (symbol (str "read-block" id)))
        block-read-fn (if-not (nil? block-read-fn-var)
                        (var-get block-read-fn-var)
                        nil)

        ;; If neither a block spec or a custom read function can be found...
        ;; We don't know how to read this block
        _ (if (and (nil? block-spec) (nil? block-read-fn))
            (throw (Throwable. "Don't know how to read block " id)))

        ;; Get the total length of the block
        length (decrypt-int (.getInt bb) (:enc-state @context))
        expected-end-position (+ (.position bb) length)

        ;; Try to read the block
        ;; If a custom read function was provided, use that
        ;; Otherwise, try to read using a block spec
        block-data (if block-read-fn
                     (block-read-fn bb context)
                     (s/read-struct block-spec bb context))

        ;; Verify we've reached the expected position
        _ (assert (= expected-end-position (.position bb)))

        ;; Verify we have the correct enc-state at this point
        checksum (Integer/toUnsignedLong (.getInt bb))
        ;; _ (assert (= checksum (:enc-state @context)))
        ]

    (assoc block-data :block-id id)))

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

        data-version (read-int! bb enc-context)
        _ (if (not (contains? #{6 7} data-version))
            (throw (Throwable. "I can't read this gdc format!")))

        mystery-field (read-bytes! bb enc-context 16)

        block1 (read-block bb enc-context)
        block2 (read-block bb enc-context)
        block3 (read-block bb enc-context)
        block4 (read-block bb enc-context)
        block5 (read-block bb enc-context)
        ]

    block5))

#_(def r (load-character-file "/Users/Odie/Dropbox/Public/GrimDawn/main/_Hetzer/player.gdc"))
