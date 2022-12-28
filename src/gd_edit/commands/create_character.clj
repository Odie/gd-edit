(ns gd-edit.commands.create-character
  (:require [clojure.data.json :as json]
            [gd-edit.utils :as u]
            [gd-edit.io.gdc :as gdc]
            [gd-edit.app-util :as au]
            [com.rpl.specter :as s]
            [gd-edit.commands.class :as class-cmds]

            [gd-edit.globals :as globals]
            [repl]

            [gd-edit.skill :as skill]))


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

(defn from-grimtools-character-file [json-file]
  (let [;; This is the character we want to end up with
        target-character (json/read-json (slurp (u/expand-home json-file)) true)

        ;; Here's a character template to work off of
        character (-> (au/locate-character-files "Odie")
                      first
                      gdc/load-character-file

                      skill/skills-remove-all)

        ;; add all classes to the character
        character (gt-apply-classes (gt-class-names target-character) character)
        character (gt-apply-attributes (gt-attributes target-character) character)
        character (gt-apply-skills (:skills target-character) character)
        ]

    character
    )
  )

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
                      )

        ]
    character
    )

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


  (json/read-json (slurp (u/expand-home "~/inbox/testChar-formatted.json")) true)

  (:skills (from-grimtools-character-file "~/inbox/testChar-formatted.json"))

  (repl/cmd "class")


  (-> @globals/character
      (select-keys [:physique :cunning :spirit])
      )

  )
