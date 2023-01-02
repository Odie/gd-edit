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
            [gd-edit.game-dirs :as dirs]))


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
    (update character :attribute-points #(max (- %
                                                 (attribute-points-required (:physique character))
                                                 (attribute-points-required (:cunning character))
                                                 (attribute-points-required (:spirit character)))
                                              0))))

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

  (let [skill-record (dbu/record-by-name (:skill-name skill))
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
  [gt-item item]

  ;; Try to look up the augment specified in the gt-item by name
  (if-let [relic-record (get (dbu/relics) (get-in gt-item [:component :name]))]
    ;; If the augment can be found, add it to the item now
    (-> item
        (assoc :relic-name (:recordname relic-record))
        (assoc :relic-seed (rand-int Integer/MAX_VALUE)))

    ;; Otherwise, just passback the unaltered item
    item))

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
                                  (gt-apply-item-relic gt-item))]

                    ;; Try to place the item onto the character
                    (if-let [updated-character (place-item-in-inventory character path item)]
                      ;; If the update failed for some reason, just return the original (un-altered) character
                      updated-character
                      character))))))

          character gt-character-equipments))

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
                       (cap-min-to-zero :skill-points))]
    character))


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
        character-dir (io/file save-dir (format "_%s" (:character-name new-character)))
        ]

    ;; The template directory now contains the new character
    ;; Move it to the local save dir now
    (.renameTo tmp-dir character-dir)))


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

  (repl/cmd "class")
  (repl/cmd "show equipment")

  (repl/cmd "show weaponsets")

  (:weapon-sets t)

  (:skills t)

  (repl/cmd "show skills")

  (repl/cmd "level 100")


  )
