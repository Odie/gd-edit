(ns gd-edit.progress-bar
  (:require [progress.util :refer [padded-printr]]
            [gd-edit.utils :as u]))

(defn progress-bar
  "Returns a string representing a progress bar. PERCENT should be an int 0 - 100"
  [percent]

  (let [percent (u/clamp percent 0 100)
        barlen (int (/ percent 2))]
    (str "["
         (apply str (repeat barlen "=")) ">"
         (apply str (repeat (- 50 barlen) " "))
         "] "
         (int percent) "%")))

(defn display-progress
  "Prints a progress bar"
  [current-size final-size]
  (if (or (nil? final-size) (<= final-size 0))
    (padded-printr (str current-size))
    (padded-printr
     (str
      (progress-bar (* 100 (/ current-size final-size)))
      " "
      current-size
      " / "
      final-size))))
