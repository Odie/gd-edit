(ns gd-edit.db-utils
  (:require [clojure.string :as str]
            [gd-edit.utils :as u]
            [gd-edit.globals :as globals]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn deref-or-exception
  "Check if `r` satisfies `pred`. If so, return dereferenced `r`, otherwise, throw an exception."
  [r pred error-msg error-map]

  (let [data @r]
    (if (not (pred data))
      (throw
       (ex-info error-msg error-map))
      data)))

(defn db
  "Returns the loaded game db, or throws an error if the db is not loaded"
  []

  (deref-or-exception globals/db #(not (empty? %))
                      "Game DB has not been loaded"
                      {:cause :db-not-loaded}))

(defn db-recordname-index
  "Returns the index, or throws an error if the db is not loaded"
  []

  (deref-or-exception globals/db-index #(not (empty? %))
                      "Game DB has not been loaded"
                      {:cause :db-not-loaded}))

(defn db-and-index
  []

  (deref-or-exception globals/db-and-index #(not (empty? %))
                      "Game DB has not been loaded"
                      {:cause :db-not-loaded}))

(defn localization-table
  []
  (deref-or-exception globals/db-and-index #(not (empty? %))
                      "Game DB has not been loaded"
                      {:cause :db-not-loaded}))

(defn record-by-name
  [recordname]

  (get (db-recordname-index) recordname))

(defn related-db-records
  [record db-and-index]

  (let [;; Collect all values in the record that look like a db record
        related-recordnames (->> record
                                 (reduce (fn [coll [key value]]
                                           (if (and (string? value) (.startsWith value "records/"))
                                             (conj coll value)
                                             coll
                                             ))
                                         #{}))

        ;; Retrieve all related records by name
        related-records (map #((:index db-and-index) %1) related-recordnames)]

    related-records))

(defn item-base-record-get-base-name
  [base-record]

  (or (get base-record "itemNameTag")
      (when-let [description (get base-record "description")]
            (str/replace description "^k" ""))))

(defn record-has-display-name?
  [record]

  (or (get record "itemNameTag") (get record "description")))

(defn item-base-record-get-quality-name
  [base-record]

  (or (base-record "itemQualityTag") (base-record "itemStyleTag")))

(defn item-base-record-get-name
  [item-base-record]

  (let [base-name (item-base-record-get-base-name item-base-record)
        quality-name (item-base-record-get-quality-name item-base-record)]

    (->> [quality-name base-name]
         (filter #(not (nil? %1)))
         (str/join " "))))

(defn- is-valid-item?
  [item]

  (if (or  (nil? (:basename item)) (empty? (:basename item)))
    false
    true))

(defn item-name
  [item db-and-index]

  (if-not (is-valid-item? item)
    nil

    (let [related-records (related-db-records item db-and-index)
          base-record (-> (filter #(= (:basename item) (:recordname %1)) related-records)
                          (first))

          base-name (item-base-record-get-base-name base-record)

          is-set-item (some #(contains? %1 "itemSetName") related-records)]

      ;; If we can't find a base name for the item, this is not a valid item
      ;; We can't generate a name for an invalid item
      (if (not (nil? base-name))

        ;; If we've found an item with a unique name, just return the name without any
        ;; prefix or suffix
        (if is-set-item
          (->> [(item-base-record-get-quality-name base-record) base-name]
               (filter some?)
               (str/join " "))

          ;; Otherwise, we should fetch the prefix and suffix name to construct the complete name
          ;; of the item
          (let [prefix-name (-> (filter #(str/includes? (:recordname %1) "/prefix/") related-records)
                                (first)
                                (get "lootRandomizerName"))
                suffix-name (-> (filter #(str/includes? (:recordname %1) "/suffix/") related-records)
                                (first)
                                (get "lootRandomizerName"))
                quality-name (item-base-record-get-quality-name base-record)]

            (->> [prefix-name quality-name base-name suffix-name]
                 (filter #(not (nil? %1)))
                 (str/join " "))))))))


;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
(defn without-meta-fields
  [kv-pair]

  (not (.startsWith (str (first kv-pair)) ":meta-")))

(defn is-primitive?
  [val]

  (or (number? val) (string? val) (u/byte-array? val) (boolean? val)))

(defn is-item?
  "Does the given collection look like something that represents an in-game item?"
  [coll]

  (and (associative? coll)
       (contains? coll :basename)))

(defn is-skill?
  "Does the given collection look like something that represents an in-game skill?"
  [coll]

  (and (associative? coll)
       (contains? coll :skill-name)
       (not (contains? coll :item-equip-location))))

(defn is-faction?
  [coll]

  (and (associative? coll)
       (contains? coll :faction-value)))

(defn skill-name
  [skill]

  (let [record (-> (:skill-name skill)
                   (record-by-name))
        record-display-name (fn [record]
                              (or (record "skillDisplayName")
                                  (record "FileDescription")))]
    (if-let [display-name (record-display-name record)]
      display-name

      (when (get record "buffSkillName")
        (record-display-name (record-by-name (get record "buffSkillName")))))))

(defn- faction-name-from-loc-table
  [loc-table faction-index]

  (when (>= faction-index 6)
    (when-let [faction-str ((localization-table) (str "tagFactionUser" (- faction-index 6)))]
      (when-not (str/starts-with? faction-str "User")
        faction-str))))

(defn faction-name
  [index]

  (let [faction-names {1  "Devil's Crossing"
                       2  "Aetherials"
                       3  "Chthonians"
                       4  "Cronley's Gang"}]
    (or
     (faction-names index)
     (faction-name-from-loc-table (localization-table) index))))

(defn item-is-materia?
  [item]

  (and (str/starts-with? (:basename item) "records/items/materia/")
       (= ((record-by-name (:basename item)) "Class") "ItemRelic")))

(defn get-type
  "Given a map in the character sheet, what type of thing does it look like?"
  [obj]

  (cond
    (is-item? obj) :item
    (is-skill? obj) :skill
    (is-faction? obj) :faction

    :else
    nil))

(defn get-name
  [obj path]

  (cond
    (is-item? obj)
    (item-name obj (db-and-index))

    (is-skill? obj)
    (skill-name obj)

    (is-faction? obj)
    (faction-name (last path))

    :else
    nil))

(defn db-get-subset
  "Get all records that matches the given path.
  Note that we'll have to walk through the entire db once."
  [db path]

  (filter #(str/starts-with? (:recordname %) path)
          db))

(defn db-get-sibling-records
  [db path]

  (db-get-subset (db)
                 (-> path
                     (u/filepath->components)
                     (drop-last)
                     (u/components->filepath-unix-style)
                     (str "/"))))

(defn item-affix-or-base-display-name
  [affix-or-base-record]

  (or (get affix-or-base-record "lootRandomizerName")
      (item-base-record-get-base-name affix-or-base-record)))

(defn record-variants
  [db record-path]

  (let [siblings (db-get-sibling-records (db) record-path)
        target-name (item-affix-or-base-display-name (record-by-name record-path))]

    (filter #(= target-name (item-affix-or-base-display-name %)) siblings)))


(defn unique-item-record-fields
  "Given a collection of item record hashmaps, filter out non-unique fields for each of the record
  in the collection. Return a collection of hashmaps."
  [item-records]

  (let [variants (if (set? item-records)
                   item-records
                   (set item-records))

        required-fields #{"levelRequirement"}

        strip-required-fields (fn [record-as-set]
                                (->> record-as-set
                                     (remove #(contains? required-fields (first %)))
                                     (set)))

        ;; Figure out which fields seems unique
        unique-fields (for [candidate variants]
                        (let [other-variants (remove #(= % candidate) variants)]
                          (apply set/difference
                                 candidate
                                 (map strip-required-fields other-variants))))
        ]

    (->> unique-fields

         ;; Remove fields that are not very interesting when looking at items
         (map #(remove (fn [variant] (contains? #{"bitmap" "glowTexture" "baseTexture" "FileDescription"} (key variant))) %))

         ;; Turn the unique fields of each variant into a hashmap again
         (map #(into {} %)))))
