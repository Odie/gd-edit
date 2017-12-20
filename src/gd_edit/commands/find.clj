(ns gd-edit.commands.find
  (:require [gd-edit.commands.help :as help]
            [gd-edit
             [globals :as globals]
             [utils :as utils]
             [db-utils :as dbu]
             [db-query :as query]
             [printer :as printer]]
            [clojure.string :as str]
            [clj-fuzzy.metrics :as metrics]
            [jansi-clj.core :refer :all]))

(defn- print-usage
  []

  (println (->> (help/get-help-item "find")
                (help/detail-help-text))))

(defn- get-name
  [obj path]

  (cond
    (dbu/is-item? obj)
    (dbu/item-name obj @globals/db-and-index)

    (dbu/is-skill? obj)
    (dbu/skill-name obj)

    (dbu/is-faction? obj)
    (dbu/faction-name (last path))

    :else
    nil))


(defn- find-handler-
  [a-name]

  ;; Grab a list of all objects that can have a display name
  (let [candidates (->> (utils/collect-walk-first-in-branch #(dbu/get-type %)
                                                            (-> @globals/character
                                                                (dissoc :meta-block-list)
                                                                (dissoc :hotslots)))
                        ;; Attach the display name to the object
                        (map (fn [[path data]]
                               {:path path
                                :data data
                                :name (get-name data path)})))

        ;; Filter against the name we're looking for
        partial-matches (filter #(utils/ci-match % a-name) candidates)]

    ;; Display the results
    (doseq [item partial-matches]
      (println
       (format "%s: %s"
               (yellow (:name item))
               (printer/displayable-path (:path item)))))))


(defn find-handler
  [[input tokens]]

  (if (< (count tokens) 1)
    (print-usage)
    (find-handler- (first tokens))))
