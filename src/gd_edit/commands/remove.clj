(ns gd-edit.commands.remove
  (:require [gd-edit.structure-walk :as sw]
            [gd-edit.globals :as globals]
            [clojure.string :as str]
            [gd-edit.printer :as printer]
            [gd-edit.commands.show :as show]
            [com.rpl.specter :as s]
            [clojure.core.match :refer [match]]
            [gd-edit.utils :as u]
            [jansi-clj.core :refer [red yellow]]))

(defn remove-handler
  [[_ [path :as tokens]]]

  (let [path-components (str/split path #"/")
        all? (when (= (last path-components) "*")
               true)
        path-components (if all?
                          (butlast path-components)
                          path-components)
        {:keys [found-item actual-path] :as result} (sw/walk @globals/character path-components)]
    (when (show/print-traverse-walk-error-or-true path-components result)
      (let [parent-path (butlast actual-path)
            parent (get-in @globals/character parent-path)
            target-path actual-path
            target found-item
            kase (match [actual-path]
                        [([_] :seq)] :no-remove
                        [([:weapon-sets _] :seq)] :fixed-length
                        [([:spawn-points _] :seq)] :fixed-length
                        [([:current-respawn _] :seq)] :fixed-length
                        [([:teleporter-points _] :seq)] :fixed-length
                        [([:markers _] :seq)] :fixed-length
                        [([:shrines _] :seq)] :fixed-length
                        [([:skill-sets _] :seq)] :fixed-length
                        [([:hotslots _] :seq)] :fixed-length
                        [([:greatest-monster-killed _] :seq)] :fixed-length
                        [([:boss-kills _] :seq)] :fixed-length
                        [([:tokens-per-difficulty _] :seq)] :fixed-length
                        [([:equipment _] :seq)] :fixed-length
                        [([:weapon-sets _ :items _] :seq)] :fixed-length
                        :else :ok)]
        (cond
          (map? parent)
          (u/print-line (red "Sorry,") "a structure field cannot be removed")

          (number? target)
          (u/print-line (red "Sorry,") "can't target a number for removal")

          (boolean? target)
          (u/print-line (red "Sorry,") "can't target a boolean for removal")

          ;; Want to remove the contents of a whole array?
          (and all?
               (sequential? target))
          (do
            (swap! globals/character #(s/transform actual-path empty %))
            (u/print-line (yellow (format "Removed %d items from \"%s\"" (count target) (u/keywords->path target-path)))))

          ;; Can't remove the array itself
          (sequential? target)
          (u/print-line (red "Sorry,") "can't target an array for removal")

          (= kase :no-remove)
          (u/print-line (red "Sorry,")
                   (format "\"%s\" cannot be removed"
                           (u/keywords->path actual-path)))

          (= kase :fixed-length)
          (u/print-line (red "Sorry,")
                   (format "\"%s\" needs to be fixed to length of %d"
                           (u/keywords->path parent-path)
                           (count parent)))

          ;; Want to remove a single item?
          (map? target)
          (do
            (swap! globals/character #(s/setval actual-path s/NONE %))
            (u/print-line (yellow (format "Removed item at \"%s\"" (u/keywords->path target-path)))))

          (and
           (string? target)
           (vector? parent))
          (do
            (swap! globals/character update-in parent-path u/vec-remove (nth actual-path 2))
            (u/print-line (yellow (format "Removed item at \"%s\"" (u/keywords->path target-path)))))

          :else
          (u/print-line (red "Oops!") "I don't know how to remove this!"))))))
