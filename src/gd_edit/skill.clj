(ns gd-edit.skill
  (:require [gd-edit.utils :as u]
            [gd-edit.db-utils :as dbu]
            [clojure.string :as str]
            [gd-edit.printer :as printer]
            [gd-edit.globals :as globals]))

(defn skills-remove-all
  [character]

  (update-in character [:skills] #(->> (take 5 %)
                                       (into (empty %)))))

(defn class-skills
  "Given a class-record, return a map of {'skill-name' => 'skill recordname'}"
  [class-record]

  (let [skill-fields (->> class-record
                          (dbu/record-fields-starting-with "skillname" ))]
    (->> skill-fields
         vals
         (map (fn [skill-recordname] [(->> skill-recordname
                                           dbu/record-by-name
                                           dbu/skill-name-from-record)
                                      skill-recordname]))
         (into {}))))

(defn player-selectable-skills
  []

  (let [;; Start at some root player record
        root-record (dbu/record-by-name "records/creatures/pc/malepc01.dbr")

        ;; Grab a list of all available classes
        class-recordnames (vals (dbu/record-fields-starting-with "skilltree" root-record))


        all-class-skills (->> class-recordnames

                              ;; For each class record...
                              (map #(->> %
                                         ;; Look up the record
                                         dbu/record-by-name

                                         ;; Return a mapping {'skillname' => 'skill recordname'}
                                         class-skills)
                                   )

                              ;; Merge all the maps into a large {skillname => skill-recordname}
                              (apply merge))]

    all-class-skills))

(defn skill-existing-entry
  [character skill-recordname]
  (some (fn [skill-entry]
          (when (= (:skill-name skill-entry) skill-recordname)
            skill-entry))
        (:skills character)))

(def blank-skill
  {:skill-name               ""
   :level                    1    ;; Skills with level 0 are ignored by the game
   :enabled                  true
   :devotion-level           0
   :devotion-experience      0
   :sublevel                 0
   :skill-active             false
   :skill-transition         false
   :autocast-skill-name      ""
   :autocast-controller-name ""})

(defn skill-add-by-display-name
  ([character skill-display-name]
   (skill-add-by-display-name skill-display-name 1))

  ([character skill-display-name level]
   (let [;; Retrieve a mapping of all skills
         all-skills (u/lower-cased-keys (player-selectable-skills))

         ;; Try to figure out the recordname that should be added to the skills array
         recordname (get all-skills (str/lower-case skill-display-name))
         _ (assert recordname)

         ;; Maybe the skill was already part of the array?
         ;; If so, where is it in the array?
         existing-entry-idx (u/first-match-position #(= (:skill-name %) recordname) (:skills character))

         ;; Grab the existing array or a map that represents an empty skill
         entry (if existing-entry-idx
                 (get-in character [:skills existing-entry-idx])
                 blank-skill)

         ;; Update the entry to something we want
         entry (-> entry
                   (assoc :skill-name recordname)
                   (assoc :level (max 1 level)))]

     ;; Put the update entry back into the array
     ;; Or just push the item onto the back of the array
     (-> (if existing-entry-idx
           (update-in character [:skills existing-entry-idx] (constantly entry))
           (update-in character [:skills] conj entry))
         (update :skill-points #(- % level))))))

(defn skill-add
  [character skill]
  (let [;; Maybe the skill was already part of the array?
        ;; If so, where is it in the array?
        existing-entry-idx (u/first-match-position #(= (:skill-name %) (:skill-name skill)) (:skills character))]

        (if existing-entry-idx
          character
          (-> character
              (update-in [:skills] conj skill)
              (update :skill-points #(- % (:level skill)))))))

(comment

  (player-selectable-skills)

  (printer/print-object
   (:skills (skill-add-by-display-name @globals/character "Raise Skeletons" 999))
   nil
   )


  (let [skills-by-name (->> (db)
                            ;; Grab a list of all known player skills
                            (reduce #(if (.startsWith (:recordname %2) "records/skills/playerclass")
                                       (conj % %2)
                                       %)
                                    (list))
                            (group-by skill-name-from-record)
                            (filter #(some? (key %)))
                            (into {})
                            )
        ]

    (->> skills-by-name
         (s/transform [s/MAP-VALS s/ALL] :recordname)
         ;; (filter #(> (count (val %)) 1))
         (into {})
         )

    ;; (->> skills-by-name
    ;;      (s/transform [s/MAP-VALS] count)
    ;;      (filter #(> (val %) 1))
    ;;      (into {})
    ;;      )


    ;; (-> skills-by-name
    ;;     (get "Storm Spirit"))

    )

  (player-selectable-skills)

  )
