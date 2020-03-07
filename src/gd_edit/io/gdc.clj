(ns gd-edit.io.gdc
  (:require [gd-edit.structure :as s]
            [clojure.java.io :as io]
            [gd-edit.utils :as u]
            [clojure.inspector]
            [clojure.pprint :refer [pprint]])
  (:import  [java.nio ByteBuffer ByteOrder]
            [java.io FileOutputStream]))

(def ^:dynamic *debug* false)

(defmacro after-block-version
  "Only return the body (else nil) if the block version is greater than or equal to `version`."
  [version body]

  `(s/conditional
    (fn [data# context#]
      (let [;; where can we find the version data?
            version-data# (if (= (:mode @context#) :write)
                            (peek (:anchor-stack @context#))
                            data#)]
        (cond
          (>= (:version version-data#) ~version)
          ~body

          :else
          nil)))))

(defmacro between-block-version
  "Only return the body (else nil) if the block version is greater than or equal to `version`."
  [v1 v2 body]

  `(s/conditional
    (fn [data# context#]
      (let [;; where can we find the version data?
            version-data# (if (= (:mode @context#) :write)
                            (peek (:anchor-stack @context#))
                            data#)]
        (cond
          (and
           (>= (:version version-data#) ~v1)
           (<= (:version version-data#) ~v2))
          ~body

          :else
          nil)))))

;;(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(declare read-block  read-byte!  read-int!  read-bool!  read-float!  read-bytes!  read-string!
         write-block write-byte! write-int! write-bool! write-float! write-bytes! write-string!)

(def FilePreamble
  (s/struct-def
   :magic   :int32
   :version :int32))

(def Header
  (s/struct-def
   :character-name    (s/string :utf-16-le)
   :male              :bool
   :player-class-name (s/string :ascii)
   :character-level   :int32
   :hardcore-mode     :bool
   :expansion-character? :byte))

(def Block1
  (s/struct-def
   :version                :int32
   :in-main-quest          :bool
   :has-been-in-game       :bool
   :last-difficulty        :byte
   :greatest-difficulty-completed :byte
   :iron                   :int32
   :greatest-survival-difficulty-completed :byte
   :tributes               :int32
   :ui-compass-state       :byte
   :always-show-loot       (between-block-version 2 4 :int32)
   :show-skill-help        :bool
   :alt-weapon-set         :bool
   :alt-weapon-set-enabled :bool
   :player-texture         (s/string :ascii)
   :loot-filters           (after-block-version 5 (s/array :byte))

   ;; Push this item onto a stack as we read the structure,
   ;; so we can figure out its version and perform conditional reading on fields
   {:anchor true}))

(def Block2
  (s/struct-def
   :version                :int32
   :level-in-bio           :int32
   :experience             :int32
   :attribute-points       :int32
   :skill-points           :int32
   :devotion-points        :int32
   :total-devotion-points-unlocked :int32
   :physique               :float
   :cunning                :float
   :spirit                 :float
   :health                 :float
   :energy                 :float))


(def Item
  (s/struct-def
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

   :relic-completion-level :int32
   :stack-count            :int32))

(def InventoryItem
  (into Item
        (s/struct-def
         :X :int32
         :Y :int32)))

(def StashItem
  (into Item
        (s/struct-def
         :X :int32
         :Y :int32)))

(def EquipmentItem
  (into Item
        (s/struct-def
         :attached :bool)))


(def InventorySack
  (s/struct-def
   :unused :bool
   :inventory-items (s/array InventoryItem)))


(defn read-block3
  [^ByteBuffer bb context]

  (let [version (read-int! bb context)
        has-data (read-bool! bb context)]
    (if-not has-data
      {:version           version
       :has-data          has-data}

      (let [sack-count (read-int! bb context)
            focused-sack (read-int! bb context)
            selected-sack (read-int! bb context)

            inventory-sacks (reduce (fn  [accum _]
                                      (conj accum (read-block bb context {0 InventorySack})))
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
                                   (range 2))]
        {:version           version
         :has-data          has-data
         :sack-count        sack-count
         :focused-sack      focused-sack
         :selected-sack     selected-sack
         :inventory-sacks   inventory-sacks
         :use-alt-weaponset use-alt-weaponset
         :equipment         equipment
         :weapon-sets       [{:unused alternate1
                              :items alternate1-set}
                             {:unused alternate2
                              :items alternate2-set}]}))))

(defn write-block3
  [^ByteBuffer bb block context]

  (write-int! bb (:version block) context)
  (write-bool! bb (:has-data block) context)

  (when (:has-data block)
    (write-int! bb (:sack-count block) context)
    (write-int! bb (:focused-sack block) context)
    (write-int! bb (:selected-sack block) context)

    (assert (= (:sack-count block) (count (:inventory-sacks block))))
    (doseq [sack (:inventory-sacks block)]
      (write-block bb sack context {0 InventorySack}))

    (write-bool! bb (:use-alt-weaponset block) context)

    (assert (=  (count (:equipment block)) 12))
    (doseq [item (:equipment block)]
      (s/write-struct EquipmentItem bb item context))

    (write-bool! bb (get-in block [:weapon-sets 0 :unused]) context)
    (assert (=  (count (get-in block [:weapon-sets 0 :items])) 2))
    (doseq [item (get-in block [:weapon-sets 0 :items])]
      (s/write-struct EquipmentItem bb item context))

    (write-bool! bb (get-in block [:weapon-sets 1 :unused]) context)
    (assert (=  (count (get-in block [:weapon-sets 1 :items])) 2))
    (doseq [item (get-in block [:weapon-sets 1 :items])]
      (s/write-struct EquipmentItem bb item context))))


;; For reference only
(def Block3
  (s/struct-def
   :version           :int32
   :has-data          :bool
   :sack-count        :int32
   :focused-sack      :int32
   :selected-sack     :int32
   :inventory-sacks   :ignore
   :use-alt-weaponset :bool
   :equipment         :ignore
   :weapon-sets       (s/struct-def
                       :unused :bool
                       :items (s/array EquipmentItem
                                       :length 2))
   {:struct/read read-block3
    :struct/write write-block3}))


(def Stash
  (s/struct-def
   :width  :int32
   :height :int32

   :items  (s/array StashItem)))

(defn read-block4
  [^ByteBuffer bb context]

  (let [version (read-int! bb context)
        stash-count (read-int! bb context)

        stashes (reduce (fn  [accum _]
                          (conj accum (read-block bb context {0 Stash})))
                        []
                        (range stash-count))]
    {:version version
     :stashes stashes}))

(defn write-block4
  [^ByteBuffer bb block context]

  (write-int! bb (:version block) context)
  (write-int! bb (count (:stashes block)) context)

  (doseq [stash (:stashes block)]
    (write-block bb stash context {0 Stash})))

(def Block4
  (s/struct-def
   {:struct/read read-block4
    :struct/write write-block4}))

(def UID
  (s/string :bytes :length 16))


(def Block5
  (s/struct-def

   :version :int32

   :spawn-points (s/array
                  (s/array UID)
                  :length 3)

   :current-respawn (s/array UID
                             :length 3)
))

(def Block6
  (s/struct-def
   :version :int32

   :teleporter-points (s/array
                       (s/array UID)
                       :length 3)
   ))

(def Block7
  (s/struct-def
   :version :int32

   :markers (s/array
             (s/array UID)
             :length 3)
))

(def Block17
  (s/struct-def
   :version :int32

   :shrines (s/array
             (s/array UID)
             :length 6)))

(def CharacterSkill
  (s/struct-def
   :skill-name               (s/string :ascii)
   :level                    :int32
   :enabled                  :bool
   :devotion-level           :int32
   :devotion-experience      :int32
   :sublevel                 :int32
   :skill-active             :bool
   :skill-transition         :bool
   :autocast-skill-name      (s/string :ascii)
   :autocast-controller-name (s/string :ascii)))

(def ItemSkill
  (s/struct-def
   :skill-name               (s/string :ascii)
   :autocast-skill-name      (s/string :ascii)
   :autocast-controller-name (s/string :ascii)
   :unknown-bytes            (s/string :ascii :length 4)
   :unknown                  (s/string :ascii)))

(def Block8
  (s/struct-def
   :version :int32

   :skills                     (s/array CharacterSkill)
   :masteries-allowed          :int32
   :skill-points-reclaimed     :int32
   :devotion-points-reclaimed  :int32
   :item-skills                (s/array ItemSkill)))

(def Block12
  (s/struct-def
   :version :int32
   :lore-item-names (s/array
                     (s/string :ascii))))

(def Faction
  (s/struct-def
   :faction-changed  :bool
   :faction-unlocked :bool
   :faction-value    :float
   :positive-boost   :float
   :negative-boost   :float))

(def Block13
  (s/struct-def
   :version        :int32
   :my-faction     :int32
   :faction-values (s/array Faction)))

(defn read-hotslot
  [^ByteBuffer bb context]

  (let [type (read-int! bb context)]
    (cond
      (= type 0)
      {:type type
       :skill-name (read-string! bb context)
       :is-item-skill (read-bool! bb context)
       :item-name (read-string! bb context)
       :item-equip-location (read-int! bb context)}

      (= type 4)
      {:type type
       :item-name (read-string! bb context)
       :bitmap-up (read-string! bb context)
       :bitmap-down (read-string! bb context)
       :default-text (read-string! bb context {:encoding :utf-16-le})}

      :else
      {:type type})))

(defn write-hotslot
  [^ByteBuffer bb hotslot context]

  (let [type (:type hotslot)]
    (write-int! bb type context)

    (cond
      (= type 0)
      (do
        (write-string! bb (:skill-name hotslot) context)
        (write-bool! bb (:is-item-skill hotslot) context)
        (write-string! bb (:item-name hotslot) context)
        (write-int! bb (:item-equip-location hotslot)context))

      (= type 4)
      (do
        (write-string! bb (:item-name hotslot) context)
        (write-string! bb (:bitmap-up hotslot) context)
        (write-string! bb (:bitmap-down hotslot) context)
        (write-string! bb (:default-text hotslot) context {:encoding :utf-16-le})))))

(def HotSlot
  (s/struct-def
   :skill-name          (s/string :ascii)
   :item-name           (s/string :ascii)
   :bitmap-up           (s/string :ascii)
   :bitmap-down         (s/string :ascii)
   :default-text        (s/string :ascii)
   :type                :int32
   :item-equip-location :int32
   :is-item-skill       :bool
   {:struct/read read-hotslot
    :struct/write write-hotslot}))

(def Block14
  (s/struct-def
   :version                :int32
   :equipment-selection    :bool
   :skill-window-selection :int32
   :skill-setting-valid    :bool

   :skill-sets             (s/array
                            (s/struct-def
                             :primary-skill   (s/string :ascii)
                             :secondary-skill (s/string :ascii)
                             :skill-active    :bool)
                            :length 5)

   :hotslots                (s/conditional
                             (fn [data _]
                               (cond
                                 (= (:version data) 4)
                                 (s/array HotSlot :length 36)
                                 :else
                                 (s/array HotSlot :length 46))))
   :camera-distance        :float))

(def Block15
  (s/struct-def
   :version :int32
   :tutorials-unlocked (s/array :int32)))

(def GreatestMonsterKilled
  (s/struct-def
   :name               (s/string :ascii)
   :level              :int32
   :life-mana          :int32
   :last-monster-hit   (s/string :ascii)
   :last-monster-hitBy (s/string :ascii)))

(def Block16
  (s/struct-def
   :version                 :int32
   :playtime-seconds        :int32
   :death-count             :int32
   :kill-count              :int32
   :experience-from-kills   :int32
   :health-potions-used     :int32
   :energy-potions-used     :int32
   :max-level               :int32
   :hits-received           :int32
   :hits-inflicted          :int32
   :crits-inflicted         :int32
   :crits-received          :int32
   :greatest-damage-done    :float

   :greatest-monster-killed (s/array GreatestMonsterKilled :length 3)

   :champion-kills              :int32
   :last-monster-hit-DA         :float
   :last-monster-hit-OA         :float
   :greatest-damage-received    :float
   :hero-kills                  :int32
   :items-crafted               :int32
   :relics-crafted              :int32
   :tier2-relics-crafted        :int32
   :tier3-relics-crafted        :int32
   :devotion-shrines-unlocked   :int32
   :one-shot-chests-unlocked    :int32
   :lore-notes-collected        :int32

   :boss-kills                  (s/array :int32 :length 3)

   :survival-greatest-wave      :int32
   :survival-greatest-score     :int32
   :survival-defense-built      :int32
   :survival-powerups-activated :int32

   :skills-map (after-block-version 11
                                    (s/array
                                     (s/struct-def
                                      :skill-name    (s/string :ascii)
                                      :level         :int32)))

   :endless-souls (after-block-version 11 :int32)

   :endless-essence (after-block-version 11 :int32)

   :difficulty-skip (after-block-version 11 :byte)

   :unique-items-found          :int32
   :randomized-items-found      :int32
   {:anchor true}))


(def Block10
  (s/struct-def
   :version               :int32

   :tokens-per-difficulty (s/array
                           (s/array
                            (s/string :ascii))
                           :length 3)))

(defn unsigned-long
  [val]

  (if (= (type val) java.lang.Integer)
    (Integer/toUnsignedLong val)
    val))

(defn enc-next-state-with-byte
  "Given a single byte, the current encryption state, and an encryption table, return the next enc state"
  [byte-data enc-state enc-table]

  (bit-and 0x00000000ffffffff
           (bit-xor enc-state (enc-table (Byte/toUnsignedInt byte-data)))))

(defn enc-next-state-with-byte-array
  "Given a byte array, the current encryption state, and an encryption table, return the next enc state"
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
  (if (or (sequential? byte-data)
          (= u/byte-array-type (type byte-data)))

    (enc-next-state-with-byte-array byte-data enc-state enc-table)

    (enc-next-state-with-byte byte-data enc-state enc-table)))

(defn decrypt-bytes!
  [^bytes buffer context]
  (loop [i 0
         limit (count buffer)
         enc-state (:enc-state context)
         enc-table (:enc-table context)]

    ;; Finished transforming the byte array?
    (if (>= i limit)
        ;; Return the new enc-state
        enc-state


      ;; Not done?
      ;; Grab the next byte, decrypt it, store it back in the same spot
      (let [enc-val (aget buffer i)
            val (byte ^long (bit-and 0x00000000000000ff (bit-xor enc-val enc-state)))]

        (aset buffer i val)
        (recur (inc i) limit (enc-next-state enc-val enc-state enc-table) enc-table)
        ))))

(defn encrypt-bytes!
  [^bytes buffer context]
  (loop [i 0
         limit (count buffer)
         enc-state (:enc-state context)
         enc-table (:enc-table context)]

    ;; Finished transforming the byte array?
    (if (>= i limit)
      ;; Return the new enc-state
      enc-state

      ;; Not done?
      ;; Grab the next byte, decrypt it, store it back in the same spot
      (let [val (aget buffer i)
            enc-val (byte ^long (bit-and 0x00000000000000ff (bit-xor val enc-state)))]

        (aset buffer i enc-val)
        (recur (inc i) limit (enc-next-state enc-val enc-state enc-table) enc-table)
        ))))

(defn transform-bytes!
  "Decrypts the given byte array in place while updating the encryption context"
  [buffer context-atom]

  (let [[transform-fn transform-buffer] (if (= :write (:direction @context-atom))
                                          [encrypt-bytes! (byte-array buffer)]
                                          [decrypt-bytes! buffer])
        next-enc-state (transform-fn transform-buffer @context-atom)]

    ;; Update the context with the new state
    (reset! context-atom (assoc @context-atom :enc-state next-enc-state))

    transform-buffer))

(defn- read-bytes-
  [^ByteBuffer bb context byte-count]

  (let [buffer (byte-array byte-count)]
    (.get bb buffer 0 byte-count)

    [buffer (decrypt-bytes! buffer context)]))

(defn- write-bytes-
  [^ByteBuffer bb data context]

  (let [encrypted-data (byte-array data)
        next-enc-state (encrypt-bytes! encrypted-data context)]
    (.put bb encrypted-data 0 (count encrypted-data))
    next-enc-state))

(defn- buffer-size-for-string
  [length encoding]

  (cond
    (= encoding :utf-16-le)
    (* length 2)

    :else
    length))

(defn read-string!
  ([^ByteBuffer bb context]
   (read-string! bb context {:static-length -1 :encoding :ascii}))

  ([^ByteBuffer bb
    context
    {:keys [static-length encoding]
     :or {static-length -1
          encoding :ascii}}]

   (let [valid-encodings {:ascii "US-ASCII"
                          :utf-8 "UTF-8"
                          :utf-16-le "UTF-16LE"}

         ;; What is the length of the string we want to read?
         ;; If a static length has been configured, use it
         ;; Otherwise, read the length
         char-count (if (not= static-length -1)
                      static-length
                      (read-int! bb context))

         byte-count (buffer-size-for-string char-count encoding)]

     ;; Read the bytes and turn it into a string
     (String. ^bytes (read-bytes! bb context byte-count) ^String (valid-encodings encoding)))))


(defn write-string!
  ([^ByteBuffer bb data context]
   (write-string! bb data context {:static-length -1 :encoding :ascii}))

  ([^ByteBuffer bb data context {:keys [static-length encoding]
                                 :or {static-length -1 encoding :ascii}}]

   (assert (not (nil? data)))

   (let [valid-encodings {:ascii "US-ASCII"
                          :utf-8 "UTF-8"
                          :utf-16-le "UTF-16LE"}

         ;; Grab the string's byte representation
         str-bytes (if (= encoding :bytes)

                     ;; If we're just looking at some raw bytes, we don't have to do anything...
                     (bytes data)

                     ;; Otherwise, we're looking at some kind of string
                     ;; Get the raw bytes after converting the string to the right encoding
                     (->> (encoding valid-encodings)
                          (java.nio.charset.Charset/forName)
                          (.getBytes data)))

         ;; Write out the length of the string itself, unless it is of a static length,
         ;; in which case, we'll say the length is implicit.
         _ (when (= static-length -1)
             ;; What is the string length we're writing out to file?
             (let [claimed-str-length (count data)]
               (write-int! bb claimed-str-length context)))

         ;; If the context asked for bytes to be transformed, do it now
         str-bytes (transform-bytes! str-bytes context)]

     ;; The string has now been converted to the right encoding and transformed.
     ;; Write it out now.
     (.put bb str-bytes))))

(defn- read-byte-
  "Retrieve the next byte, decrypt the value, then return the value and the next enc state"
  [^ByteBuffer bb {:keys [enc-state enc-table]}]

  (let [enc-val (.get bb)]
    [(short (bit-and 0x00000000000000ff (bit-xor enc-val enc-state))) (enc-next-state enc-val enc-state enc-table)]))

(defn- write-byte-
  [^ByteBuffer bb data {:keys [enc-state enc-table]}]

  ;; encrypt the value and write it
  (let [enc-val (byte (bit-and 0x00000000000000ff (bit-xor data enc-state)))
        _ (.put bb enc-val)

        next-enc-state (enc-next-state enc-val enc-state enc-table)]

    next-enc-state))

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

(def encrypt-int decrypt-int)

(defn- read-int-
  "Retrieve the next byte, decrypt the value, then return the value and the next enc state"
  [^ByteBuffer bb {:keys [enc-state enc-table]}]

  (let [enc-val (.getInt bb)
        byte-data (-> (ByteBuffer/allocate 4)
                      (.putInt enc-val)
                      (.array)
                      (reverse))]

    ;; Return [decrypted-value next-enc-state] pair
    [(decrypt-int enc-val enc-state) (enc-next-state byte-data enc-state enc-table)]))


(defn- write-int-
  [^ByteBuffer bb data {:keys [enc-state enc-table]}]

  ;; encrypt the value and write it
  (let [enc-val (encrypt-int data enc-state)
        _ (.putInt bb enc-val)

        ;; use the updated value to update the encryption state
        byte-data (-> (ByteBuffer/allocate 4)
                      (.order ByteOrder/LITTLE_ENDIAN)
                      (.putInt enc-val)
                      (.array))

        next-enc-state (enc-next-state byte-data enc-state enc-table)]

    next-enc-state))

(defn- read-bool-
  [^ByteBuffer bb context]

  (let [[val next-enc-state] (read-byte- bb context)]
    [(= val 1) next-enc-state]))

(defn- write-bool-
  [^ByteBuffer bb data context]

  (assert (or (= data true) (= data false)))
  (write-byte- bb (if data 1 0) context))

(defn- read-float-
  "Retrieve the next byte, decrypt the value, then return the value and the next enc state"
  [^ByteBuffer bb {:keys [enc-state enc-table]}]

  ;; Java doesn't allow bit-wise manipulation for floats
  ;; We'll have to read a 4 byte quantity, decrypt it, then reinterpret it as a float
  (let [enc-val (.getInt bb)
        byte-data (-> (ByteBuffer/allocate 4)
                      (.putInt enc-val)
                      (.array)
                      (reverse))
        reinterpreter (ByteBuffer/allocate 8)]

    [(-> reinterpreter
         ;; Decrypt the bit pattern
         ;; And re-interpret the bits as a float
         (.putLong (bit-and 0x00000000ffffffff (bit-xor enc-val enc-state)))
         (.flip)
         (.position 4)
         (.getFloat))

     (enc-next-state byte-data enc-state enc-table)]))

(defn- write-float-
  [^ByteBuffer bb data context]

  (write-int- bb
              (-> (ByteBuffer/allocate 4)
                  (.putFloat data)
                  (.flip)
                  (.getInt))
              context))

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

(defn write-and-update-context
  [write-fn ^ByteBuffer bb data context & rest]

  ;; Run the write function
  (let [new-enc-state (apply write-fn bb data @context rest)]

    ;; Update the enc-state
    (reset! context (assoc @context :enc-state new-enc-state))

    ;; Return the read value
    val))

(def write-bytes! (partial write-and-update-context write-bytes-))
(def write-byte!  (partial write-and-update-context write-byte-))
(def write-bool!  (partial write-and-update-context write-bool-))
(def write-int!   (partial write-and-update-context write-int-))
(def write-float! (partial write-and-update-context write-float-))

(defn make-enc-context
  [enc-state enc-table & [options-map]]

  (atom (merge (or options-map {})
               {;; Mutable state
                :enc-state enc-state
                :enc-table enc-table

                ;; Specs table for the structure lib
                :rw-fns {:transform-bytes! transform-bytes!
                         :byte   [:byte    1 read-byte!  write-byte! ]
                         :bool   [:byte    1 read-bool!  write-bool! ]
                         :int32  [:int32   4 read-int!   write-int!  ]
                         :float  [:float   4 read-float! write-float!]}})))

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
                     (int
                      (unchecked-multiply
                       (long (bit-or (bit-shift-left val 31) (bit-shift-right val 1)))
                       (long 39916801))))]

        (recur (inc i)
               limit
               new-val
               (conj! table new-val))))))


(defn validate-preamble
  [preamble]

  (when (or (not= (:magic preamble) (u/file-magic "GDCX"))
          ;; (not= (:version preamble) 2)
          ;; (not= (:minor-version preamble) 4)
          )
    (throw (Throwable. "I don't understand this gdc format!"))))


(defn get-block-spec-by-ns
  "Retrieve a block spec by id number"
  [ns id]

  (when-let [block-spec-var (ns-resolve ns (symbol (str "Block" id)))]
    (println "block-spec fetched, id:" id)
    (var-get block-spec-var)))

(defn get-block-read-fn-by-ns
  "Retrieve a read function for block by id number"
  [ns id]

  (let [block-read-fn-var (ns-resolve ns (symbol (str "read-block" id)))]

    (if-not (nil? block-read-fn-var)
      (do
        (println "read-fn fetched, id:" id)
        (var-get block-read-fn-var))
      nil)))

(defn get-block-write-fn-by-ns
  "Retrieve a read function for block by id number"
  [ns id]

  (let [block-write-fn-var (ns-resolve ns (symbol (str "write-block" id)))]

    (if-not (nil? block-write-fn-var)
      (var-get block-write-fn-var)
      nil)))

(defn get-block-spec
  [block-specs id]
  ;; This function doesn't do a lot. It's just a place to insert some debugging
  ;; info when needed.
  (get block-specs id))

(def block-specs
  "A map containing block id => block structure def.

  Note that we're depending some magic/convention here.
  We're expecting there is a var in this namespace named 'BlockX' where
  X is an integer id of the block.

  This allows us to repeat less of ourselves. We're also less likely to
  make a typo in the mapping and end up debugging things for a hour."

  (into {}
        (for [id [1 2 3 4 5 6 7 8 10 12 13 14 15 16 17]]
          (when-let [block-spec-var (resolve (symbol (str "Block" id)))]
            [id (var-get block-spec-var)]))))

(defn read-block-header
  [^ByteBuffer bb context]

  {:id (read-int! bb context)
   :length (read-int! bb context)})

(defn read-block
  ([^ByteBuffer bb context]
   (read-block bb context nil))

  ([^ByteBuffer bb context block-specs]

   (let [;; Read the block id
         id (read-int! bb context)

         ;; Get the total length of the block
         length (decrypt-int (.getInt bb) (:enc-state @context))
         expected-end-position (+ (.position bb) length)

         ;; Figure out how we can read the block
         block-spec-or-read-fn (get-block-spec block-specs id)

         ;; If neither a block spec or a custom read function can be found...
         ;; We don't know how to read this block
         _ (when (nil? block-spec-or-read-fn)
             (throw (Throwable. (str"Don't know how to read block " id))))

         _ (when *debug*
             (println "read-block--------------")
             (println "id" id)
             (println "length" length)
             (println "expected-end-position" expected-end-position)
             (println "actual-position" (.position bb)))

         ;; Try to read the block
         ;; If a custom read function was provided, use that
         ;; Otherwise, try to read using a block spec
         block-data (s/read-struct block-spec-or-read-fn bb context)

         _ (when *debug*
             (println "block-data")
             (pprint block-data))

         ;; Verify we've reached the expected position
         _ (assert (= expected-end-position (.position bb)))

         ;; Verify we have the correct enc-state at this point
         checksum (Integer/toUnsignedLong (.getInt bb))
         _ (assert (= checksum (:enc-state @context)))
         ]

     (assoc block-data :meta-block-id id))))

(defn write-block
  ([^ByteBuffer bb block context]
   (write-block bb block context nil))

  ([^ByteBuffer bb block context block-specs]
   {:pre [(not (nil? block))]}

   (let [{:keys [meta-block-id]} block
         block-spec (get-block-spec block-specs meta-block-id)

         ;; If neither a block spec or a custom read function can be found...
         ;; We don't know how to write this block
         _ (when (nil? block-spec)
             (throw (Throwable. (str "Don't know how to write block: " meta-block-id))))

         ;; Write the id of the block
         _ (write-int! bb meta-block-id context)

         ;; Write a dummy block length
         ;; We'll come back and fill it back in when we know how long the block is
         length-field-pos (.position bb)
         length-field-enc-state (:enc-state @context)
         _ (.putInt bb 0)

         ;; Write the block using either a custom write function or the block spec
         _ (s/write-struct block-spec bb block context)

         ;; Write out the real length of the block
         block-end-pos (.position bb)
         block-length (- (.position bb) length-field-pos 4)
         _ (.position bb length-field-pos)
         _ (.putInt bb (int (bit-and 0x00000000ffffffff (bit-xor block-length length-field-enc-state))))
         _ (.position bb block-end-pos)

         ;; Write the checksum to end the block
         _ (.putInt bb (int (bit-and 0x00000000ffffffff (:enc-state @context))))]
     )))

(defn block-strip-meta-info-fields
  [block]

  (->> block
       (filter (fn [[key _]]
                 (not (contains? #{:version :meta-block-id} key))))
       (into {})))

(defn load-character-file
  [filepath]

  (let [bb ^ByteBuffer (u/file-contents filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        seed (bit-xor (Integer/toUnsignedLong (.getInt bb)) 1431655765)
        enc-table (generate-encryption-table seed)
        enc-context (make-enc-context seed enc-table)

        _ (when *debug*
            (println "\n---------------------- Preamble ----------------------"))
        preamble (s/read-struct FilePreamble bb enc-context)
        _ (when *debug*
            (pprint preamble))

        _ (validate-preamble preamble)

        _ (when *debug*
            (println "\n---------------------- Header ----------------------"))
        header (assoc (s/read-struct Header bb enc-context) :meta-block-id :header)
        _ (when *debug*
            (pprint header))

        header-checksum (Integer/toUnsignedLong (.getInt bb))
        _ (assert (= header-checksum (:enc-state @enc-context)))

        data-version (read-int! bb enc-context)
        _ (when (not (contains? #{6 7 8} data-version))
            (throw (Throwable. "I can't read this gdc format!")))

        mystery-field (read-bytes! bb enc-context 16)

        fileinfo {:seed seed
                  :preamble preamble
                  :data-version data-version
                  :mystery-field mystery-field}

        ;; Keep reading more blocks until we've reached the end of the file
        block-list (->> (loop [block-list (transient [])]
                          (if (= (.remaining bb) 0)
                            (persistent! block-list)

                            (recur (conj! block-list (read-block bb enc-context block-specs)))))

                        ;; Append the header block when we're done reading
                        (into [header]))

        ;; Try to merge all the block lists into one giant character sheet
        character (assoc (apply merge (map block-strip-meta-info-fields block-list))
                         :meta-block-list block-list
                         :meta-fileinfo fileinfo
                         :meta-character-loaded-from filepath)
        ]
    character))

(defn write-to-file
  [bb filepath]

  (with-open [file-channel (-> (io/file filepath)
                               (FileOutputStream.)
                               (.getChannel))]
    (.write file-channel bb)
    (.force file-channel true)
    (.close file-channel)))

(defn get-block
  [block-list block-id]

  (first (filter #(= (:meta-block-id %1) block-id) block-list)))

(defn update-block-list
  "Update the `block-list` with potentially new values from the `merged-map`.

  The idea here is that the `merged-map` was generated as a merged view of all
  the items in the `block-list`, sans various pieces of meta data. The
  `merged-map` can then be used as the primary piece of data to be viewed and
  updated/altered.


  This was originally created for dealing with the character file. The
  'character sheet' was created using the block-list by merging all blocks into
  a giant dictionary. To re-create the block list from a character sheet then,
  we need to update the top level kv pair of every block to the current value in
  the character sheet.

  When it's time to actually write the file, however, the block-list needs to
  have all of their fields updated before being written back to a file. This
  function performs that reverse mapping."
  [block-list merged-map]

  (map (fn [block]
         ;; Map over every kv in the blocks
         (->> block
              (map (fn [[k _ :as kv-pair]]
                     ;; Look up the updated value in the character sheet
                     (if (contains? merged-map k)
                       [k (merged-map k)]
                       kv-pair)))
              ;; Put the pairs back into a map
              (into {})))
       block-list))

(defn write-character-file
  [character savepath]

  (let [bb (ByteBuffer/allocate (* 512 1024))
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        ;; Grab the original block list
        ;; This was kept when we loaded the save file in the order
        ;; the blocks were read
        block-list (:meta-block-list character)

        ;; Create a new block list when the updated contents
        ;; The character sheet was created using the block-list by merging
        ;; all blocks into a giant dictionary.
        ;; To re-create the block list from a character sheet then, we need to
        ;; update the top level kv pair of every block to the current value in
        ;; the character sheet.
        updated-block-list (update-block-list block-list character)

        fileinfo (:meta-fileinfo character)

        seed (:seed fileinfo)
        enc-table (generate-encryption-table seed)
        enc-context (make-enc-context seed enc-table {:direction :write})

        _ (.putInt bb (bit-xor seed 1431655765))
        _ (s/write-struct FilePreamble bb (:preamble fileinfo) enc-context)
        _ (s/write-struct Header bb (get-block updated-block-list :header) enc-context)
        _ (.putInt bb (int ^long (:enc-state @enc-context)))
        _ (write-int! bb (:data-version fileinfo) enc-context)
        _ (write-bytes! bb (fileinfo :mystery-field) enc-context)

        ]

    (doseq [block (->> updated-block-list
                       (filter #(not= (:meta-block-id %1) :header)))]
      (write-block bb block enc-context block-specs))

    (.flip bb)

    (write-to-file bb savepath)))
