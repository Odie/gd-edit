(ns gd-edit.commands.find
  (:require [clj-fuzzy.metrics :as metrics]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [gd-edit.commands.help :as help]
            [gd-edit.db-query :as query]
            [gd-edit.db-utils :as dbu]
            [gd-edit.game-dirs :as dirs]
            [gd-edit.globals :as globals]
            [gd-edit.io.gdc :as gdc]
            [gd-edit.printer :as printer]
            [gd-edit.utils :as utils]
            [jansi-clj.core :refer :all]))

(defn- print-usage
  []

  (println (->> (help/get-help-item "find")
                (help/detail-help-text))))


(defn- load-character-file-safe
  [filepath]

  (try
    (gdc/load-character-file filepath)
    (catch Error e
      (println (str "Unable to read file: " filepath)))))

(defn- load-all-characters
  "Loads all characters that the editor can currently locate and read."
  []

  (->>
   (dirs/get-all-save-file-dirs)
   (map #(.getPath (io/file % "player.gdc")))
   (map #(load-character-file-safe %))
   (filter some?)))

(defn character-data-with-names
  [character]

  (->> (utils/collect-walk-first-in-branch #(dbu/get-type %)
                                           (-> character
                                               (dissoc :meta-block-list)
                                               (dissoc :hotslots)))
       ;; Attach the display name to the object
       (map (fn [[path data]]
              {:path path
               :data data
               :name (dbu/get-name data path)}))))

(defn quest-data-with-names
  "Collect all items in the quest progress data tree that has a :name field.
   The :name field is an annotation added during character load."
  [character]

  (->> (utils/collect-walk-entire-tree #(:name %)
                                       (character :quest))

       (map (fn [[path data]]
              (let [real-path (into [:quest] path)]
                {:path real-path
                 :data data
                 :name (:name data)})))))

(defn- find-all-handler-
  "Locates anything that partially matches the `a-name` for all locate-able characters"
  [a-name]

  (let [interesting-data-items
        (->> (load-all-characters)
             (mapcat (fn [character]
                       (->> (concat (character-data-with-names character) (quest-data-with-names character))
                            (map #(assoc % :character character))))))

        partial-matches (filter #(utils/ci-match (:name %) a-name) interesting-data-items)]

    (doseq [[char items] (group-by :character partial-matches)]
      (println "character: " (yellow (:meta-character-loaded-from char)))

      (doseq [item items]
        (println
         (format "%s: %s"
                 (yellow (:name item))
                 (printer/displayable-path (:path item)))))

      (println)
      )
    )
  )

(defn- find-handler-
  "Locates anything that partially matches the `a-name` for the currently loaded character"
  [a-name]

  ;; Grab a list of all objects that can have a display name
  (let [candidates (character-data-with-names @globals/character)

        ;; We want to also search quests and tasks by name
        quest-candidates (quest-data-with-names @globals/character)

        ;; Filter against the name we're looking for
        partial-matches (filter #(utils/ci-match (:name %) a-name) (concat candidates quest-candidates))]

    ;; Display the results
    (doseq [item partial-matches]
      (println
       (format "%s: %s"
               (yellow (:name item))
               (printer/displayable-path (:path item)))))))


(defn find-handler
  [[input tokens]]

  (cond
    (< (count tokens) 1)
    (print-usage)

    (and (>= (count tokens) 2)
         (= (first tokens) "all"))
    (find-all-handler- (second tokens))

    :else
    (find-handler- (first tokens))))


(comment

  )
