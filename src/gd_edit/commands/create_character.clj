(ns gd-edit.commands.create-character
  (:require [clojure.data.json :as json]
            [gd-edit.utils :as u]
            [gd-edit.io.gdc :as gdc]
            [gd-edit.app-util :as au]
            [com.rpl.specter :as s]
            [gd-edit.commands.class :as class-cmds]
            [gd-edit.globals :as globals]
            [gd-edit.skill :as skill]
            [gd-edit.commands.item :as item]
            [gd-edit.commands.level :as level]
            [gd-edit.db-utils :as dbu]
            [clojure.java.io :as io]

            [clojure.pprint :refer [pprint]]
            [me.raynes.fs :as fs]
            [gd-edit.jline :as jl]
            [gd-edit.game-dirs :as dirs]
            [gd-edit.printer :as printer]
            [gd-edit.db-query :as query]
            [clojure.string :as str]))


(defn gt-classes
  [gt-character]

  (:classes gt-character))

(defn gt-attributes
  [gt-character]
  (:attributes gt-character))

(defn gt-apply-classes
  [gt-character-classes character]

  (reduce #(class-cmds/class-add-by-name % (:name %2) (:level %2)) character gt-character-classes))

(defn attribute-points-required
  "For any attribute, calcuate how many attribute points are required to reach the given target value.

  Example: How many skill points does it need to raise physique to 450?
  => 50"
  [target-val]

  (-> target-val
      (- 50)
      (/ 8)))

(defn gt-apply-attributes
  [gt-character-attributes character]

  (let [character (cond-> character
                    (:physique gt-character-attributes)
                    (assoc :physique (:physique gt-character-attributes))

                    (:cunning gt-character-attributes)
                    (assoc :cunning (:cunning gt-character-attributes))

                    (:spirit gt-character-attributes)
                    (assoc :spirit (:spirit gt-character-attributes))
                    )]
    (update character :attribute-points #(dbu/coerce-to-type
                                          (max (- %
                                                  (attribute-points-required (:physique character))
                                                  (attribute-points-required (:cunning character))
                                                  (attribute-points-required (:spirit character)))
                                               0)
                                          (type %)))))

(defn gt-apply-skills
  [gt-character-skills character]

  ;; Entries in the gt-character-skills array may have a single layer of child skills.
  ;; Flatten that into a single list so we can add the skills more easily.
  (let [gt-character-skills (->> gt-character-skills
                                 (s/select [s/ALL :children])
                                 (apply concat gt-character-skills))]

    (reduce (fn [character gt-skill]
              (skill/skill-add-by-display-name
               character
               (:name gt-skill)
               (:level gt-skill)))
            character
            gt-character-skills)))

(defn devotion-skill-set-max-level
  [skill]

  (let [skill-record (dbu/devotion-skill-descriptor-by-recordname (:skill-name skill))
        max-level (get skill-record "skillMaxLevel")
        exp-levels (get skill-record "skillExperienceLevels")
        level (max 1 (count exp-levels) (or max-level 0))
        exp (or (last exp-levels) 0)

        display-name (dbu/skill-display-name skill-record)]

    (when (and display-name
               (> level 1))
      (println (format "Setting '%s' to level %d"
                       display-name
                       level)))

    (-> skill
        (assoc :devotion-level level)
        (assoc :devotion-experience exp))))

(defn gt-apply-devotions
  [gt-devotions character]

  (let [skills-map (dbu/constellation-skills-map)]
    (reduce (fn [character gt-devotion]

              (let [skill-recordname (-> skills-map
                                            (get {:constellation-id (:constellationNumber gt-devotion )
                                                  :button-id (:devotionButton gt-devotion )})
                                            dbu/record-by-name
                                            (get "skillName"))]

                (if (not skill-recordname)
                  (do
                    (println "Oops... unable to locate this devotion in the character file...")
                    (pprint gt-devotion)
                    character)

                  (let [skill-record (dbu/record-by-name skill-recordname)
                        ]
                    (-> character
                        (update :skills conj (-> skill/blank-skill
                                               (assoc :skill-name skill-recordname)
                                               (devotion-skill-set-max-level)))
                        (update :devotion-points dec))))))
            character
            gt-devotions)))

(def weapon-set-path {:weapon1 [:weapon-sets 0 :items 0]
                      :weapon2 [:weapon-sets 0 :items 1]
                      :weapon1Alt [:weapon-sets 1 :items 0]
                      :weapon2Alt [:weapon-sets 1 :items 1]})

(def equipment-slot-idx {:head   0
                         :amulet 1
                         :chest  2
                         :legs   3
                         :feet   4
                         :hands  5
                         :ring1  6
                         :ring2  7
                         :waist  8
                         :shoulders 9
                         :medal  10
                         :relic  11})

(def equipment-slot-path
  (merge
   weapon-set-path
   (into {}
         (for [[slot-name idx] equipment-slot-idx]
           [slot-name [:equipment idx]]))))

(defn place-item-in-inventory
  [character path item]

  (if-let [[updated-character actual-path] (item/place-item-in-inventory character path item)]
    updated-character
    character))

(defn gt-apply-item-augment
  [gt-item item]

  ;; Try to look up the augment specified in the gt-item by name
  (if-let [augment-record (get (dbu/augments) (get-in gt-item [:augment :name]))]
    ;; If the augment can be found, add it to the item now
    (-> item
        (assoc :augment-name (:recordname augment-record))
        (assoc :augment-seed (rand-int Integer/MAX_VALUE)))

    ;; Otherwise, just passback the unaltered item
    item))


(defn gt-apply-item-relic
  "Apply relic/component settings to the item"
  [gt-item item]

  ;; Try to look up the augment specified in the gt-item by name
  (if-let [relic-record (get (dbu/relics) (get-in gt-item [:component :name]))]
    ;; If the augment can be found, add it to the item now
    (-> item
        (assoc :relic-name (:recordname relic-record))
        (assoc :relic-seed (rand-int Integer/MAX_VALUE)))

    ;; Otherwise, just passback the unaltered item
    item))


(defn relic-search-by-name
  [relic-name]
  (first (query/query-db (dbu/db) (format "Class=\"ItemArtifact\" value~\"%s\"" relic-name))))

(defn relic-completion-bonus-records
  [relic-record]

  (assert (= (get relic-record "Class") "ItemArtifact"))

  (let [completion-records (as-> relic-record $
                                  (get $ "bonusTableName")
                                  (dbu/record-by-name $)
                                  (dbu/record-fields-starting-with "randomizerName" $)
                                  (map val $)
                                  (map dbu/record-by-name $)
                                  )]
    completion-records))

(defn select-keys-by
  [m pred]

  (->> m
       (reduce (fn [accum kv]
                 (if (pred (key kv))
                   (conj accum kv)
                   accum))
               [])
       (into {})))

(defn relic-bonus-v-normalize
  "We need a way to compare matched relic bonus fields, between what's on a grimtools character and
  what's in the dabase.

  Here, we normalize the representation of the value field so it's easier to determine their equality.
  "
  [v]

  (cond
    (nil? v) v

    ;; Single strings are returned as is
    (string? v) v

    ;; Integers are returned as floats
    (int? v) (float v)

    ;; Floats are returned as is
    (float? v) v

    ;; Vectors should be considered as a set
    (or (vector? v) (list? v)) (set v)

    :else
    (ex-info "Don't know how to handle relic bonus value of this type!")))

(defn relic-bonus-v=
  [a b]

  (= (relic-bonus-v-normalize a) (relic-bonus-v-normalize b)))

(defn relic-completion-bonus--match-kv
  "Given a list of relic completion bonus records, and a single k v pair to look for, return the
  record that matches the given `kv`"
  [completions [k v]]

  (reduce (fn [res item]
            (when (relic-bonus-v= (get item k) v)
              (reduced item)))
          nil
          completions))


(defn relic-completion-bonus--match-kvs
  "Given a list of relic completion bonus records, and mutiple k v pairs, `kvs`, to match for, return the
  record that matches the given all kvs pairs"
  [completions kvs]

  (reduce (fn [res item]
            (when (every? (fn [[k v]] (relic-bonus-v= (get item k) v)) kvs)
              (reduced item)))
          nil
          completions))

(defn relic-completion-bonus-skill-augments
  [relic-record]

  (let [bonus-records (relic-completion-bonus-records relic-record)
        augment-records (filter #(dbu/record-fields-starting-with "augmentSkillLevel" %) bonus-records)]
    (apply hash-map
           (interleave
            (->> augment-records
                 ;; Select only kv pairs where the key starts with 'augmentSkillName'
                 (map #(select-keys-by % (fn [k]
                                           (str/starts-with? (str k) "augmentSkillName"))))

                 ;; Each value point to a skill to be augmented
                 ;; Look up the record and turn it into the actual skill name
                 (s/transform [s/ALL s/MAP-VALS] #(dbu/skill-name-from-record (dbu/record-by-name %)))

                 ;; Collect the skill names into a set
                 (map #(set (vals %))))
            (map :recordname augment-records)))))

(defn gt-apply-artifact-completion-bonus
  "Apply the completion bonus for a thing in the `relic` equipment slot"
  [gt-item item]

  (if-not (= "relic" (:slot gt-item))
    item

    (let [relic-record (dbu/record-by-name (:basename item))
          bonus-spec (get-in gt-item [:completionBonus :completionBonuses])
          bonus-recordname (if (get-in gt-item [:completionBonus :isClassRelic])
                             (get (relic-completion-bonus-skill-augments relic-record) (set bonus-spec))

                             ;; The bonus spec should be a bunch of kv pairs that needs to all match to
                             ;; uniquely identify a single bonus record for this relic

                             ;; Make sure our key names are strings instead of keywords
                             ;; This needs to happen because the database uses strings as keys
                             (->> (s/transform [s/MAP-KEYS] name bonus-spec)
                                  ;; Grab a list of bonus records
                                  ;; Try to find one that matches all of the criteria
                                  (relic-completion-bonus--match-kvs (relic-completion-bonus-records relic-record))
                                  :recordname))]
      (cond-> item
        bonus-recordname (assoc :relic-bonus bonus-recordname))
      )))

(defn gt-apply-equipment
  [gt-character-equipments character]

  ;; Try to add each piece of equipment onto the character
  (reduce (fn [character gt-item]
            ;; Try to construct the item
            (let [slot-name (:slot gt-item)
                  path (equipment-slot-path (keyword slot-name))
                  _ (println (str "Generating item for: " slot-name))

                  character-level (:character-level character)
                  item-name (:name gt-item)

                  ;; Try to to create the requested item at the character level
                  item (item/construct-item item-name character-level)

                  ;; if that's not possible, try to create then item with no level restrictions
                  item (if (some? item)
                         item
                         (do
                           (println (format "Could not create item matching character level: %d" character-level))
                           (println "Trying again with no level restrictions")
                           (item/construct-item item-name nil)))
                  ]

              (cond

                ;; If it's just impossible to create the item, do nothing and return the character unaltered
                (or (nil? item)
                    (not (item/item-names-similiar (:name gt-item) (dbu/item-name item))))
                (do
                  (println (format "Could not create item: %s" item-name))
                  character)

                :else
                (do
                  (let [item (->> item
                                  (gt-apply-item-augment gt-item)
                                  (gt-apply-item-relic gt-item)
                                  (gt-apply-artifact-completion-bonus gt-item))
                        ;; Some gt-items may come with specifc prefix and suffix recordnames
                        item (cond-> item
                                 (:prefix gt-item) (assoc :prefix-name (:prefix gt-item))
                                 (:suffix gt-item) (assoc :suffix-name (:suffix gt-item)))
                        ]

                    ;; Try to place the item onto the character
                    (if-let [updated-character (place-item-in-inventory character path item)]
                      ;; If the update failed for some reason, just return the original (un-altered) character
                      updated-character
                      character))))))

          character gt-character-equipments))

(comment

  ;; Test generating a single item
  (let [equipments (-> (json/read-json (slurp (u/expand-home "~/inbox/charData-13-f.json")) true)
                       :items
                       (nth 8)
                       (vector))
        character (gdc/load-character-file (io/file (io/resource "_blank_character/player.gdc")))]

    (def t (gt-apply-equipment equipments character))
    )

  (-> t
      :equipment
      (nth 6)
      )

  )

;; Given some equipment definition that comes from gt directly...
;; Refine an existing already generated item...
;;
;; Expects input in the shape of:
;;
;; {:item "records/items/gearrelic/d201_relic.dbr",
;;  :relicBonus "records/items/lootaffixes/completionrelics/aoathkeeper_19a.dbr"}
(defn gt-equipment-refine
  "Given some equipment definition that comes from gt directly, refine an existing item.
  More specifically, make sure we are using the exact prefix and suffix"
  [gt-character-data-equipment item]

  (let [mappings {:item :basename
                  :prefix :prefix-name
                  :suffix :suffix-name
                  :component :relic-name
                  :augment :augment-name}]
    (reduce (fn [item [def-k new-val]]
              (cond-> item
                  ;; If we know apply a definition onto a field...
                  (mappings def-k)
                  ;; Put the new value into the correct corresponding field
                  (assoc (mappings def-k) new-val)))
            item
            gt-character-data-equipment)))

(defn gt-apply-equipment-refine
  [gt-character-data-equipments character]

  (reduce (fn [character [slot-name item-def]]
            (let [path (equipment-slot-path slot-name)]
              (update-in character path #(gt-equipment-refine item-def %))))
          character
          gt-character-data-equipments))

(defn prompt-set-character-name
  [character]

  (println)
  (let [character-name (jl/readline "Give the character a name: ")]
    (if (not-empty character-name)
      (update character :character-name (constantly character-name))
      character)))

(defn prompt-set-character-level
  [character]

  (binding [gd-edit.globals/character (atom character)]
    (loop []
      (println)
      (let [character-level (jl/readline "Input the character level: ")
            result (level/level-handler ["" [character-level]])]
        (if (not= result :ok)
          (recur)
          @globals/character)))))

(defn println-passthrough-last
  [text passthrough]
  (println text)
  passthrough)

(defn cap-min-to-zero
  [field-keyword character]

  (update character field-keyword #(max % 0)))

(defn from-grimtools-character-file
  [json-file template-character]
  (let [;; This is the character we want to end up with
        target-character (json/read-json (slurp json-file) true)

        ;; Apply various settings from the json character file
        character (->> template-character
                       skill/skills-remove-all
                       prompt-set-character-name
                       prompt-set-character-level
                       (println-passthrough-last "")

                       (gt-apply-classes (gt-classes target-character))
                       (gt-apply-attributes (gt-attributes target-character))

                       (gt-apply-skills (:skills target-character))
                       (gt-apply-devotions (:devotionNodes target-character))
                       (println-passthrough-last "")

                       (gt-apply-equipment (:items target-character))
                       (cap-min-to-zero :attribute-points)
                       (cap-min-to-zero :skill-points))
        ]
    (cond->> character
      ;; If we have some equipment data that comes directly from grimtools...
      ;; Apply the exact records that should be used for the equipment
      (get-in target-character [:data :equipment])
      (gt-apply-equipment-refine (get-in target-character [:data :equipment])))))


(defn gt-apply-attributes-v2
  [gt-character-attributes character]

  (let [;; gt-character field -> gd-edit character fields
        mapping {:attributePoints :attribute-points :skillPoints :skill-points
                 :devotionPoints :devotion-points
                 :physique :physique
                 :cunning :cunning
                 :spirit :spirit}]
    (reduce (fn [character [gt-attr-name new-val]]
              (cond-> character
                (mapping gt-attr-name)
                (assoc (mapping gt-attr-name) new-val)))
            character
            gt-character-attributes)))


(defn mk-skill-v2
  [gt-character-skill]

  (let [;; gt-character field -> gd-edit character fields
        mapping {:name :skill-name
                 :level :level
                 :autoCastSkill :autocast-skill-name}]
    (->> (reduce (fn [character [field-name val]]
               (cond-> character
                 (mapping field-name)
                 (assoc (mapping field-name) val)))
             skill/blank-skill
             gt-character-skill)
         (devotion-skill-set-max-level))))

(defn gt-apply-skills-v2
  [gt-character-skills character]

  (reduce (fn [character gt-character-skill]
            (update character :skills conj (mk-skill-v2 gt-character-skill)))
          character
          gt-character-skills))

(comment
  (gt-apply-skills-v2 (-> j :data :skills) {})

  (-> j :data :equipment)

  (dbu/record-by-name
   "records/skills/devotion/tier3_21c.dbr")

  (item/blank-item)

  )


(defn create-character
  "Take the json file, recreate the character using a template, then move the character to
  the local save directory
  "
  [json-filepath]
  (let [json-file (io/file json-filepath)

        ;; Copy the template character directory to a temporary location on disk
        tmp-dir (fs/temp-dir "gd-edit-char")
        _ (u/copy-resource-files-recursive "_blank_character" tmp-dir)

        ;; Load the template directory
        character-file (io/file tmp-dir "player.gdc")
        template-character (gdc/load-character-file character-file)

        ;; Create a new character from the template
        new-character (from-grimtools-character-file json-file template-character)

        ;; Save it back into the template files directory
        _ (gdc/write-character-file new-character character-file)

        save-dir (dirs/get-local-save-dir)

        _ (println)
        _ (println "local save dir seems to be: " save-dir)

        character-dir (io/file save-dir (format "_%s" (:character-name new-character)))
        ]

    ;; The template directory now contains the new character
    ;; Move it to the local save dir now
    (println "Saving character to")
    (println "\t" (.getAbsolutePath character-dir))
    (println)

    (if-not (.renameTo tmp-dir character-dir)
      (do
        (println "Moving the directory didn't seem to work...")
        (println "Copying and overwriting instead...")
        (fs/copy-dir-into tmp-dir character-dir)
        (fs/delete-dir tmp-dir)))

    (if-not (.exists character-dir)
      (println "Oops! Unable to save the character to the destination for some reason..."))))


(comment

  ;; Make a new character from a template
  (let [template (gdc/load-character-file (io/file (io/resource "_blank_character/player.gdc")))]
    (def t (from-grimtools-character-file (u/expand-home "~/inbox/charData (4).json") template)))

  (let [devotions

        (->> (json/read-json (slurp (u/expand-home "~/inbox/charData (4).json")) true)
             (:devotionNodes)
             )]
    (gt-apply-devotions devotions {:skills []
                                   :skill-points 1000
                                   :devotion-points 55
                                   })
    :ok)


  (def t (gdc/load-character-file (io/file (io/resource "_blank_character/player.gdc"))))

  ;; What does the target character look like?
  (json/read-json (slurp (u/expand-home "~/inbox/charData.json")) true)

  ;; What does the current character look like?
  (reset! globals/character t)

  @globals/character

  (let [character (-> (au/locate-character-files "Odie")
                      first
                      gdc/load-character-file
                      remove-all-skills
                      )]
    character)

  (->> (range 10)
       (drop 1)
       )

  (let [a-map {:skills [:a :b :c :d :e :f :g]}

        ]
    (s/transform [:skills] (fn [v] (take 5 v)) a-map)
    )

  (do
    (reset! globals/character
            (from-grimtools-character-file "~/inbox/testChar-formatted.json"))
    :ok
    )


  (:skills @globals/character)

  (:skills (json/read-json (slurp (u/expand-home "~/inbox/charData.json")) true))

  (let [gt-character-skills (:skills (json/read-json (slurp (u/expand-home "~/inbox/charData.json")) true))]

    (->> gt-character-skills
         (s/select [s/ALL :children])
         (apply concat gt-character-skills)
         )

    )

  (:equipment (from-grimtools-character-file (u/expand-home "~/inbox/testChar-formatted.json")))

  (-> (from-grimtools-character-file (u/expand-home "~/inbox/testChar-formatted.json"))
      (gdc/write-character-file (u/expand-home "~/inbox/out.gdc")))

  (json/read-json (slurp (u/expand-home "~/inbox/charData.json")) true)

  (create-character (u/expand-home "~/inbox/charData-f.json"))

  (class-cmds/class-display-name-map)

  (require 'repl)

  (repl/cmd "load AAA")

  (repl/cmd "class")
  (repl/cmd "show equipment")

  (repl/cmd "show weaponsets")

  (:weapon-sets t)

  (:skills t)

  (repl/cmd "show skills")

  (repl/cmd "level 100")

  (require '[gd-edit.printer :as printer])


  ;;------------------------------------------------------------------------------
  ;; Relic completion bonus forms
  ;;------------------------------------------------------------------------------
  (def t (relic-search-by-name "Deathchill"))

  (-> t
      relic-completion-bonus-records)

  (-> t
      relic-completion-bonus-records
      (relic-completion-bonus--match-kvs {"characterDexterityModifier"  3
                                          "characterIntelligenceModifier" 3
                                          "characterStrengthModifier" 3}))

  (-> t
      relic-completion-bonus-records
      (relic-completion-bonus--match-kvs {"racialBonusRace" ["Race002" "Race012"]
                                          "racialBonusPercentDamage" 8}))

  (def t (relic-search-by-name "Eye of the Storm"))

  (-> t
      relic-completion-bonus-skill-augments
      )

  (do
    (def j
      (json/read-json (slurp (u/expand-home "~/inbox/charData-11-f.json")) true))

    (def t
      (relic-search-by-name "Eye of the Storm")))

  (->> (json/read-json (slurp (u/expand-home "~/inbox/charData-11-f.json")) true)
       :items
       last
       )

  (gt-apply-artifact-completion-bonus
   (-> j
       :items
       last
       )

   {:stack-count 1,
    :basename (:recordname t)
    :unknown 0
    :relic-seed 0
    :prefix-name ""
    :relic-completion-level 0
    :seed 1699262392
    :augment-seed 0
    :transmute-name ""
    :augment-name ""
    :modifier-name ""
    :relic-name ""
    :suffix-name ""
    :attached true
    :relic-bonus ""}
   )


  (do
    (def j
      (json/read-json (slurp (u/expand-home "~/inbox/charData-12-f.json")) true))

    (def t
      (relic-search-by-name "Deathchill")))

  (->> @globals/character
       :equipment
       last
       )

  )
