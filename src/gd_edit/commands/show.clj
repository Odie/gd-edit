(ns gd-edit.commands.show
  (:require [gd-edit.utils :as u]
            [gd-edit.globals :as globals]
            [clojure.string :as str]
            [gd-edit.app-util :as au]
            [gd-edit.printer :as printer]
            [gd-edit.structure-walk :as sw]
            [gd-edit.commands.choose-character :as commands.choose-character]))

(defn- classname
  [obj]

  (cond
    (u/byte-array? obj)
    "byte array"

    (integer? obj)
    "integer"

    (float? obj)
    "float"

    (string? obj)
    "string"))

(defn print-cannot-traverse-walk-result
  [result]

  (let [actual-path-item (get-in @globals/character (:actual-path result))]
    (println (format "Cannot traverse into %s at the path \"%s\""
                     (classname actual-path-item)
                     (str/join "/" (:longest-path result))))))

(defn show-handler
  [[input tokens]]

  (cond
    ;; If the character hasn't been loaded...
    ;; Move to the character selection screen first
    (not (au/character-loaded?))
    (commands.choose-character/character-selection-screen!)

    ;; The character is loaded
    ;; If there aren't any filter conditions, just display all the character fields
    (= (count tokens) 0)
    (printer/print-map @globals/character)

    :else
    (let [;; Split the given path into components
          path-keys (str/split (first tokens) #"/")

          ;; Try to walk the path fuzzily
          result (sw/walk @globals/character path-keys)
          {:keys [status found-item]} result]

      (cond
        (= status :not-found)
        (println "No matches found")

        (= status :too-many-matches)
        (if ;; If we were able to match the entire path, but arrived at multiple entries
            (= (count (:longest-path result)) (count path-keys))

          ;; Show all matched entries
          (printer/print-map (into {} (:ambiguous-matches result)))

          ;; If we stopped before matching the entire path, that means
          ;; we had trouble stepping into one of the given items
          (sw/print-ambiguous-walk-result result))

        (= status :cannot-traverse)
        (print-cannot-traverse-walk-result result)

        :else
        (do
          ;; Print the path to the entry being shown
          ;; This helps the user verify what they're looking at is what is asked for.
          (println (->> (:actual-path result)
                        (map u/keyword->str)
                        (str/join "/")))
          (printer/print-object found-item (:actual-path result)))))))
