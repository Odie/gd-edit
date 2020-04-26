(ns gd-edit.level
  (:require [gd-edit.db-utils :as dbu]
            [gd-edit.equation-eval :as eq]))

(defn attribute-points-total-at-level
  "Return the total number of skill points a character should have at the give level."
  [level]
  {:pre [(>= level 1)]}

  (* (dec level) (-> (dbu/record-by-name "records/creatures/pc/playerlevels.dbr")
                     (get "characterModifierPoints"))))

(defn skill-points-total-at-level
  "Return the total number of skill points a character should have at the give level."
  [level]
  {:pre [(>= level 1)]}

  (apply + (take (dec level) (-> (dbu/record-by-name "records/creatures/pc/playerlevels.dbr")
                                 (get "skillModifierPoints")))))

(defn xp-total-at-level
  "Returns the number of exp points a character should have at the given level."
  [level]
  {:pre [(>= level 1)]}

  (-> (dbu/record-by-name "records/creatures/pc/playerlevels.dbr")
      (get "experienceLevelEquation")
      (eq/evaluate {"playerLevel" (dec level)})
      int))
