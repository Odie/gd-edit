(ns gd-edit.commands.level
  (:require [gd-edit.globals :as globals]
            [gd-edit.db-utils :as dbu]
            [gd-edit.printer :as printer]
            [gd-edit.level :refer :all]
            [clojure.data :refer [diff]]
            [gd-edit.utils :as u]))

(defn fields-for--modify-character-level
  [character new-level]

  (dbu/coerce-map-numbers-using-reference
   {:character-level new-level
    :level-in-bio new-level
    :max-level new-level
    :experience (xp-total-at-level new-level)
    :skill-points
    (let [points-to-award
          (- (skill-points-total-at-level new-level)
             (skill-points-total-at-level (character :character-level)))]

      (max 0 (+ (:skill-points character) points-to-award)))
    :attribute-points
    (let [points-to-award
          (- (attribute-points-total-at-level new-level)
             (attribute-points-total-at-level (character :character-level)))]
      (max 0 (+ (:attribute-points character) points-to-award)))

    :masteries-allowed (max 2 (:masteries-allowed character))}

   character))

(defn modify-character-level
  [character new-level]
  (merge character (fields-for--modify-character-level character new-level)))

(defn level-handler
  [[input tokens]]

  (cond
    (empty? tokens)
    (u/print-line
     "usage: level <new-level>")

    :else
    (let [level (dbu/coerce-str-to-type (first tokens) java.lang.Integer)
          level-limit (-> (dbu/record-by-name "records/creatures/pc/playerlevels.dbr")
                          (get "maxPlayerLevel"))]
      (cond
        (= level :failed)
        (do
          (u/print-line "That's not a valid integer")
          :level-should-be-an-integer)

        (< level 1)
        (do
          (u/print-line "Please enter a level value that is 1 or greater")
          :level-should-be-positive)

        (> level level-limit)
        (do
          (u/print-line "Sorry, max allowed level is" level-limit)
          :max-level-exceeded)

        :else
        (let [modified-character (merge @globals/character
                                        (fields-for--modify-character-level @globals/character level))]
          (u/print-line "Changing level to" level)
          (u/newline-)

          (u/print-line "Updating the following fields:")
          (printer/print-map-difference (diff @globals/character modified-character))

          (swap! globals/character modify-character-level level)
          :ok)))))
