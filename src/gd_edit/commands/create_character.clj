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


            [me.raynes.fs :as fs]
            [gd-edit.jline :as jl]
            [gd-edit.game-dirs :as dirs]))


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
                           (item/construct-item item-name nil)))]

              (if (nil? item)
                ;; If it's just impossible to create the item, do nothing and return the character unaltered
                (do
                  (println (format "Could not create item: %s" item-name))
                  character)

                ;; Try to place the item onto the character
                (if-let [updated-character (when (item/item-names-similiar (:name gt-item) (dbu/item-name item))
                                             (when-let [[updated-character actual-path] (item/place-item-in-inventory character path item)]
                                               updated-character))]
                  ;; If the update failed for some reason, just return the original (un-altered) character
                  updated-character
                  character))))

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

(defn from-grimtools-character-file
  [json-file template-character]
  (let [;; This is the character we want to end up with
        target-character (json/read-json (slurp json-file) true)

        ;; Apply various settings from the json character file
        character (->> template-character
                       skill/skills-remove-all
                       prompt-set-character-name
                       (gt-apply-classes (gt-class-names target-character))
                       (gt-apply-attributes (gt-attributes target-character))

                       prompt-set-character-level
                       (println-passthrough-last "")
                       (gt-apply-skills (:skills target-character))
                       (gt-apply-equipment (:items target-character))
                       )]
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

  (create-character (u/expand-home "~/inbox/testChar-formatted.json"))

  )
