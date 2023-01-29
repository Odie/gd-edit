(ns gd-edit.db-utils
  "Collection of functions that deals with fetching and interpreting game data."
  (:require [clojure.string :as str]
            [gd-edit.utils :as u]
            [gd-edit.globals :as globals]
            [clojure.set :as set]
            [com.rpl.specter :as s]
            [me.raynes.fs :as fs]
            ))

(defn record-class
  [record]
  (record "Class"))

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
  (deref-or-exception globals/localization-table #(not (empty? %))
                      "Game DB has not been loaded"
                      {:cause :db-not-loaded}))

(defn record-by-name
  [recordname]

  (get (db-recordname-index) recordname))

(defn record-field
  [record field-name]
  (let [v (record field-name)]
    (if-let [referenced-record (record-by-name v)]
      referenced-record
      v)))

(defn related-db-records
  ([record]
   (related-db-records record (db-and-index)))
  ([record db-and-index]

   (let [;; Collect all values in the record that look like a db record
         related-recordnames (->> record
                                  (reduce (fn [coll [_ value]]
                                            (if (and (string? value) (.startsWith value "records/"))
                                              (conj coll value)
                                              coll
                                              ))
                                          #{}))

         ;; Retrieve all related records by name
         related-records (map #((:index db-and-index) %1) related-recordnames)]

     related-records)))

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
  ([item]
   (item-name item @globals/db-and-index))
  ([item db-and-index]

   (if-not (is-valid-item? item)
     nil

     (let [related-records (related-db-records item db-and-index)
           base-record (-> (filter #(= (:basename item) (:recordname %1)) related-records)
                           (first))

           base-name (item-base-record-get-base-name base-record)

           is-set-item (some #(contains? %1 "itemSetName") related-records)]

       ;; If we can't find a base name for the item, this is not a valid item
       ;; We can't generate a name for an invalid item
       (when-not (nil? base-name)

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
                  (str/join " ")))))))))


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

(defn uid?
  [x]
  (and (u/byte-array? x)
       (= (count x) 16)))

(defn skill-display-name
  [skill-record]
  (skill-record "skillDisplayName"))

(defn skill-name-from-record
  [skill-record]

  (when skill-record
    (if-let [display-name (skill-display-name skill-record)]
      display-name

      (or
       (when-let [buff-skill (record-by-name (skill-record "buffSkillName"))]
         (skill-name-from-record buff-skill))
       (when-let [pet-skill (record-by-name (skill-record "petSkillName"))]
         (skill-name-from-record pet-skill))))))

(defn skill-description-from-record
  [skill-record]

  (let [skill-description (fn [record]
                              (record "skillBaseDescription"))]
    (if-let [description (skill-description skill-record)]
      description

      (or
       (when-let [buff-skill (record-by-name (skill-record "buffSkillName"))]
         (skill-description-from-record buff-skill))
       (when-let [pet-skill (record-by-name (skill-record "petSkillName"))]
         (skill-description-from-record pet-skill))))))

(defn skill-name
  [skill]

  (let [record (-> (:skill-name skill)
                   (record-by-name))]
    (skill-name-from-record record)))

(defn- faction-name-from-loc-table
  [loc-table faction-index]

  (when (>= faction-index 6)
    (when-let [faction-str (loc-table (str "tagFactionUser" (- faction-index 6)))]
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

(defn race-name
  [race-tag]
  (case race-tag
    "Race001" "Undead"
    "Race002" "Beastkin"
    "Race003" "Aetherial"
    "Race004" "Chthonic"
    "Race005" "Aether Corruption"
    "Race006" "Bloodsworn"
    "Race007" "Eldritch"
    "Race008" "Insectoid"
    "Race009" "Human"
    "Race010" "Construct"
    "Race011" "Riftspawn"
    "Race012" "Beast"
    "Race013" "Magical"
    "Race014" "Celestial"
    "Race015" "Arachnid "))

(defn item-is-materia?
  [item]

  (and (str/starts-with? (:basename item) "records/items/materia/")
       (= ((record-by-name (:basename item)) "Class") "ItemRelic")))

(defn uid-record-display-name
  [uid-record]

  (let [display-name (or
                      (uid-record "journalTag")
                      (uid-record "description"))]
    (if (and (= display-name "???")
             (uid-record "FileDescription"))
      (uid-record "FileDescription")
      display-name)))

(defn uid-record-type-display-name
  [uid-record]
  (let [class (uid-record "Class")]
    (cond
      (= class "StaticShrine")
      "Shrine"

      (= class "StaticTeleporter")
      "Rift Gate"

      :else
      class)))

(defn uid-display-name
  [uid]
  (when-let [uid-info (@globals/shrines-and-gates-index (seq uid))]
    (let [record (record-by-name (:recordname uid-info))]
      (uid-record-display-name record))))

(defn get-type
  "Given a map in the character sheet, what type of thing does it look like?"
  [obj]

  (cond
    (is-item? obj) :item
    (is-skill? obj) :skill
    (is-faction? obj) :faction
    (uid? obj) :uid

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

    (uid? obj)
    (uid-display-name obj)

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

  (db-get-subset db
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

  (let [siblings (db-get-sibling-records db record-path)
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

(defn clean-display-name
  [display-name]
  (str/replace display-name "^k" ""))

(defn get-cleaned-description
  [record]
  (str/replace (get record "description") "^k" ""))

(defn augments
  "Return a <augment name> => <record> map for relics/components"
  []

  (->> (db)

       ;; We're only interested in things that are classed "ItemEnchantment"
       (filter #(= (get % "Class") "ItemEnchantment"))

       ;; Group them by their display name
       (group-by get-cleaned-description)

       ;; Each augment name now corresponds to a list of records
       ;; We want to turn them into a "augment name" => record mapping
       (s/transform [s/MAP-VALS] (fn [record-list]
                                   ;; Either unwrap single element lists...
                                   (if (= 1 (count record-list))
                                     (first record-list)

                                     ;; Or try to do some basic tie-breaking
                                     (->> record-list
                                          (sort-by #(count (:recordname %)))
                                          first))))))

(defn relics
  "Return a <component name> => <record> map for relics/components"
  []
  (->> (db)
       (filter #(= (get % "Class") "ItemRelic"))
       (group-by get-cleaned-description)
       (s/transform [s/MAP-VALS] #(first %))))


;;------------------------------------------------------------------------------
;; Type coercion
;;------------------------------------------------------------------------------
(defn- parseBoolean
  [val-str]

  (let [true-aliases ["true" "t" "1"]
        false-aliases ["false" "f" "0"]]
    (cond
      (contains? (into #{} true-aliases) val-str)
      true

      (contains? (into #{} false-aliases) val-str)
      false

      :else
      (throw (Throwable.
              (str/join "\n"
                        [(format "Can't interpret \"%s\" as a boolean value" val-str)
                         "Try any of the following: "
                         (str "  true  - " (str/join ", " true-aliases))
                         (str "  false - " (str/join ", " false-aliases))]))))))

(defn coerce-str-to-type
  "Given an value as a string, return the value after coercing it to the correct type."
  [val-str type]

  (try

    (cond
      ;; If the caller asked to convert the value to a string,
      ;; we don't have to do anything because the value should already be a string
      (= java.lang.String type)
      val-str

      (= java.lang.Float type)
      (Float/parseFloat val-str)

      (= java.lang.Double type)
      (Double/parseDouble val-str)

      (= java.lang.Short type)
      (Short/parseShort val-str)

      (= java.lang.Integer type)
      (Integer/parseInt val-str)

      (= java.lang.Long type)
      (Long/parseLong  val-str)

      (= java.lang.Boolean type)
      (parseBoolean val-str))

    (catch Exception e
      :failed)))

(defn coerce-number-to-type
  [val-number to-type]

  (cond
    (= java.lang.String to-type)
    (.toString val-number)

    (= java.lang.Float to-type)
    (float val-number)

    (= java.lang.Double to-type)
    (double val-number)

    (= java.lang.Short type)
    (Short. val-number)

    (= java.lang.Integer to-type)
    (int val-number)

    (= java.lang.Long to-type)
    (long val-number)

    (= java.lang.Boolean to-type)
    (if (= val-number 0)
      false
      true)

    :else
    (throw (Throwable. (format "Don't know how to coerce %s => %s"
                               (str (type val))
                               (str to-type))))))

(defn coerce-to-type
  [val to-type]
  (cond
    (string? val)
    (coerce-str-to-type val to-type)

    (number? val)
    (coerce-number-to-type val to-type)

    :else
    (throw (Throwable. (format "Don't know how to coerce %s => %s"
                               (str (type val))
                               (str to-type))))))

(defn coerce-map-numbers-using-reference
  "Coerce all map values that are numbers to the same type as the field in the reference map."
  [m reference]

  (->> m
       (map (fn [[k v]]
              (if (number? v)
                [k (coerce-to-type v (type (reference k)))]
                [k v])))
       (into (empty m))))

(defn get-shrines
  []
  (->> @globals/shrines-and-gates
       (map (fn [m]
              (assoc m :record (record-by-name (:recordname m)))))
       (filter #(u/ci-match (get-in % [:record "Class"]) "shrine"))
       distinct
       (map #(assoc % :display-name (uid-record-display-name (:record %))))
       (sort-by :display-name)))

(defn get-gates
  []
  (->> @globals/shrines-and-gates
       (map (fn [m]
              (assoc m :record (record-by-name (:recordname m)))))
       (filter #(u/ci-match (get-in % [:record "Class"]) "teleporter"))
       distinct
       (map #(assoc % :display-name (uid-record-display-name (:record %))))
       (sort-by :display-name)))

(defn rank-by-name-similarity
  [a-name coll]
  (u/rank-by-similarity a-name :display-name coll))

(defn record-fields-starting-with
  [substring record]

  (filter #(u/starts-with-insensitive? (str (key %)) substring) record))

(defn record-has-field
  [fieldname record]

  (get record fieldname))

(defn parse-devotion-button-name
  [devotion-button-name]

  (-> (subs devotion-button-name (count "devotionButton"))
      Integer/parseInt))

(defn constellationr-record->skills-map
  [constellation-record]

  (let [record constellation-record
        constellation-id (-> (:recordname record)
                             ;; (io/file)
                             (fs/name)
                             (subs (count "constellation"))
                             Integer/parseInt)
        interesting-fields (->> (u/select-keys-pred record (fn [k]
                                                             (str/starts-with? k "devotionButton"))))]

    (->> interesting-fields
         (map (fn [[button-name skill-recordname]]
                [{:constellation-id constellation-id
                  :button-id (parse-devotion-button-name button-name)}
                 skill-recordname]
                ))
         (into {}))))

(defn constellation-skills-map []
  (->> (db)
       (filter #(str/starts-with? (get % :recordname) "records/ui/skills/devotion/constellations/"))
       (filter #(not (record-has-field "bitmapName" %)))
       (map constellationr-record->skills-map)
       (apply merge)))

(defn constellation-star-skill-recordnames
  []
  (->> (constellation-skills-map)
       vals
       (map record-by-name)
       (map #(get % "skillName"))
       set))

(def constellation-star-skill-recordnames-memoized (memoize constellation-star-skill-recordnames))

(defn devotion-skill-descriptor-by-recordname
  "Fetch a a record that actually looks like a structure that describes a skill.

  There are records in the database that are actually references to other skills.

  So instead of the expected:
  [skill name lookup] => [structure describing a skill]

  We might actually have:
  [skill name lookup] => [dummy buff skill record] => [structure describing a skill]

  This function follows that indirection to always return a structure describing a skill.
  "
  [recordname]

  (let [record (record-by-name recordname)]
    (or
     ;; If we seem to have arrived at a dummy buff skill record, continue the lookup
     (when (get record "buffSkillName")
       (record-by-name (get record "buffSkillName")))

     ;; Otherwise, just return the record
     record)))

(defn devotion-skill-descriptor-by-record
  "Fetch a a record that actually looks like a structure that describes a skill.

  There are records in the database that are actually references to other skills.

  So instead of the expected:
  [skill name lookup] => [structure describing a skill]

  We might actually have:
  [skill name lookup] => [dummy buff skill record] => [structure describing a skill]

  This function follows that indirection to always return a structure describing a skill.
  "
  [skill-record]

  (or
   ;; If we seem to have arrived at a dummy buff skill record, continue the lookup
   (when (get skill-record "buffSkillName")
     (record-by-name (get skill-record "buffSkillName")))

   ;; Otherwise, just return the record
   skill-record))

(defn celestial-power-recordnames
  []
  (->> (constellation-skills-map)
       vals
       (map record-by-name)
       (map #(get % "skillName"))
       (filter (fn [skill-recordname]
                 (let [record (devotion-skill-descriptor-by-recordname skill-recordname)]
                   (and (get record "skillDisplayName")
                        (get record "skillExperienceLevels")))))
       set))

(def celestial-power-recordnames-memoized (memoize celestial-power-recordnames))
