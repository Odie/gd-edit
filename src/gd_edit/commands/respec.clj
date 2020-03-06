(ns gd-edit.commands.respec
  (:require [clojure.string :as str]
            [gd-edit.utils :as u]
            [gd-edit.globals :as globals]
            [gd-edit.printer :as printer]
            [gd-edit.db-utils :as dbu]))

(defn skill-is-devotion?
  [skill]

  (str/starts-with? (:skill-name skill) "records/skills/devotion"))

(defn passive-skill? [skill]
  (let [record ((dbu/db-recordname-index) (:skill-name skill))]
    (= (get record "Class") "Skill_Passive")))

(defn disable-devotion-skill [skill]
  ;; If the given skill is not a devotion, do thing...
  (if-not (skill-is-devotion? skill)
    skill
    (cond-> skill
      ;; Passive skills should be marked "not enabled"
      (passive-skill? skill) (assoc :enabled false)

      ;; all skills should indicate no devotion points have been used/spent
      ;; for this skill
      :lastly (assoc :level 0))))

(defn devotion-skill-enabled? [skill]
  (if (passive-skill? skill)
    (= (:enabled skill) true)
    (> (:level skill) 0)))

(defn skills-vec->skills-by-category
  [skills-list]

  {:default (take 5 skills-list)
   :skills (->> (drop 5 skills-list)
                (filter #(not (skill-is-devotion? %))))

   :devotions (->> (drop 5 skills-list)
                   (filter skill-is-devotion? ))})

(defn skills-by-category->skills-vec
  [skills-by-category]

  (->> (concat (:default skills-by-category)
               (:skills skills-by-category)
               (:devotions skills-by-category))
       (into [])))

(defn respec-character-attributes
  [character option]

  (let [base-attr-points 50
        data-record (dbu/record-by-name "records/creatures/pc/playerlevels.dbr")]

    (merge character
           (dbu/coerce-map-numbers-using-reference
            {:physique base-attr-points
             :cunning base-attr-points
             :spirit base-attr-points

             :attribute-points
             (let [points-to-award
                   (+
                    (/ (- (:physique character) base-attr-points)
                       (data-record "strengthIncrement"))
                    (/ (- (:cunning character) base-attr-points)
                       (data-record "dexterityIncrement"))
                    (/ (- (:spirit character) base-attr-points)
                       (data-record "intelligenceIncrement")))]
               (-> (+ (:attribute-points character) points-to-award)
                   (max 0)))}
            character))))

(defn respec-character-skills
  [character option]

  (let [skills-by-category (skills-vec->skills-by-category (:skills character))
        skills (:skills skills-by-category)

        ;; Compute how many skill points were used
        points-to-award (reduce #(+ % (:level %2))
                                0
                                skills)]

    (merge character
           (dbu/coerce-map-numbers-using-reference
            {;; Refund the points used back to the character
             :skill-points (+ (:skill-points character) points-to-award)

             ;; Remove all skills from the skills vector
             :skills (skills-by-category->skills-vec
                      (dissoc skills-by-category :skills)
                      )}
            character))))

(defn respec-character-devotions
  [character options]

  ;; Grab a list of devtions taken
  (let [skills-by-category (skills-vec->skills-by-category (:skills character))
        devotions (:devotions skills-by-category)]

    (merge character
           (dbu/coerce-map-numbers-using-reference
            {;; Refund 1 devotion point per devotion point spent
             :devotion-points (+ (:devotion-points character)
                                 (->> devotions
                                      (map #(:level %))
                                      (reduce +)))

             ;; Remove or disable all devotions depending on the given option
             :skills (skills-by-category->skills-vec
                      (if (:hard options)
                        (dissoc skills-by-category :devotions)
                        (update skills-by-category
                                :devotions
                                #(map disable-devotion-skill %))))}
            character))))

(defn respec-handler
  [[input tokens]]

  (let [mode (str/lower-case (or (first tokens) "all"))
        valid-modes #{"all" "attributes" "devotions" "skills" "hard"}]

    ;; Sanity check on the respec mode
    (if-not (contains? valid-modes mode)
      (do
        (println "Please choose from one of the valid respec types:")
        (doseq [r-type (sort valid-modes)]
          (u/print-indent 1)
          (println r-type)))

      ;; Try to modify the character according to the chosen mocd
      (let [options (when (= mode "hard") {:hard true})
            modified-character
            (cond
              (or (= mode "all")
                  (= mode "hard"))

              (-> @globals/character
                  (respec-character-devotions options)
                  (respec-character-skills options)
                  (respec-character-attributes options))

              (= mode "attributes")
              (respec-character-attributes @globals/character options)

              (= mode "skills")
              (respec-character-skills @globals/character options)

              (= mode "devotions")
              (respec-character-devotions @globals/character options)

              :else
              (throw (Throwable. (str "Unknown respec mode: " mode))))

            differences (clojure.data/diff @globals/character modified-character)]

        ;; Print how we're going to update the character
        (println "Updating the following fields:")
        (printer/print-map-difference differences)

        ;; Actually update the character
        (swap! globals/character (constantly modified-character))))))
