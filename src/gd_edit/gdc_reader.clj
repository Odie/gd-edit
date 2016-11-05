(ns gd-edit.gdc-reader
  (:require [gd-edit.structure :as s]
            [gd-edit.utils :as utils]))


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
      (let [new-val (Integer/toUnsignedLong (.intValue (unchecked-multiply (long (bit-or (bit-shift-left val 31) (bit-shift-right val 1)))
                                                                    (long 39916801))))]

        (recur (inc i)
               limit
               new-val
               (conj! table new-val))))))

(defn load-character-file
  [filepath]

  (let [bb (utils/mmap filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        seed (bit-xor (.getInt bb) 1431655765)
        enc-table (generate-encryption-table seed)
        ]

    enc-table))

#_(load-character-file "/Users/Odie/Dropbox/Public/GrimDawn/main/_Hetzer/player.gdc")
