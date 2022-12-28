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
            [gd-edit.db-utils :as dbu]
            [clojure.java.io :as io]
            [repl]))


(defn gt-classes
  [gt-character]

  (:classes gt-character))

(defn gt-class-names
  [gt-character]

  (->> (gt-classes gt-character)
       (map :name)))

(defn gt-attributes
  [gt-character]
  (:attributes gt-character))

(defn gt-apply-classes
  [gt-character-classes character]

  (reduce #(class-cmds/class-add-by-name % %2) character gt-character-classes)
  )

(defn gt-apply-attributes
  [gt-character-attributes character]

  ;; FIXME? It's unclear if I need to cast this to a float here
  ;; I really should not need to perform type coersion here
  ;; It'd be better if the type is correctly coerced when it's written to file
  (cond-> character
      (:physique gt-character-attributes)
      (update :physique (constantly (:physique gt-character-attributes)))

      (:cunning gt-character-attributes)
      (update :cunning (constantly (:cunning gt-character-attributes)))

      (:spirit gt-character-attributes)
      (update :spirit (constantly (:spirit gt-character-attributes)))))

(defn gt-apply-skills
  [gt-character-skills character]

  (reduce (fn [character gt-skill]
            (skill/skill-add-by-display-name
             character
             (:name gt-skill)
             (:level gt-skill)))
          character
          gt-character-skills))

(def weapon-set-path {:weapon1 [:weapon-sets 0 :items 0]
                      :weapon2 [:weapon-sets 1 :items 0]})

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

(defn gt-apply-equipment
  [gt-character-equipments character]

  ;; Try to add each piece of equipment onto the character
  (reduce (fn [character gt-item]
            ;; Try to construct the item
            (let [slot-name (:slot gt-item)
                  path (equipment-slot-path (keyword slot-name))
                  _ (println (str "Generating item for: " slot-name))
                  item (item/construct-item (:name gt-item) (:level character))

                  ;; Try to place the item onto the character
                  updated-character (when (item/item-names-similiar (:name gt-item) (dbu/item-name item))
                                      (when-let [[updated-character actual-path] (item/place-item-in-inventory character path item)]
                                        updated-character))]
              ;; If the update failed for some reason, just return the original (un-altered) character
              (if updated-character
                updated-character
                character)))

              character gt-character-equipments)
  )

(defn from-grimtools-character-file
  [json-file]
  (let [;; This is the character we want to end up with
        target-character (json/read-json (slurp json-file) true)

        ;; Here's a character template to work off of
        ;; Grab an emtpy character file packed with the editor
        character (-> (io/file (io/resource "blank-character.gdc"))
                      gdc/load-character-file
                      skill/skills-remove-all)

        ;; Apply various settings from the json character file
        character (->> character
                       (gt-apply-classes (gt-class-names target-character))
                       (gt-apply-attributes (gt-attributes target-character))
                       (gt-apply-skills (:skills target-character))
                       (gt-apply-equipment (:items target-character)))
        ]

    character))


(comment

  (from-grimtools-character-file "~/inbox/testChar-formatted.json")

  ;; What does the target character look like?
  (json/read-json (slurp (u/expand-home "~/inbox/testChar-formatted.json")) true)

  ;; What does the current character look like?
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

  (:items (json/read-json (slurp (u/expand-home "~/inbox/testChar-formatted.json")) true))

  (:equipment (from-grimtools-character-file (u/expand-home "~/inbox/testChar-formatted.json")))

  (-> (from-grimtools-character-file (u/expand-home "~/inbox/testChar-formatted.json"))
      (gdc/write-character-file (u/expand-home "~/inbox/out.gdc")))

  )
