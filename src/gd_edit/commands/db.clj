(ns gd-edit.commands.db
  (:require [clojure.string :as str]
            [gd-edit.utils :as u]
            [gd-edit.printer :as printer]
            [gd-edit.db-utils :as dbu]))

(defn string-first-n-path-components
  "Given a path, return a path with the first n path components"
  [n path]
  (str/join "/" (take n (u/path-components path))))

(defn path-partially-matches?
  [path partial-path]
  (->> (partition 2 (interleave (u/path-components partial-path)
                                (u/path-components path)))
       (every? (fn [[target-component item-component]]
                 (u/ci-match item-component target-component)))))

(defn collect-path-partial-matches
  [paths partial-path]
  (filter #(path-partially-matches? % partial-path) paths))

(defn db-partial-record-path-matches
  [db path]
  (collect-path-partial-matches (map :recordname db) path))

(defn db-show-handler
  [[input tokens]]

  (let [path (if (nil? (first tokens))
               "r/"
               (first tokens))

        components (u/path-components path)

        ;; Grab a list of items from the db that partially matches the specified path
        matches (db-partial-record-path-matches (dbu/db) path)

        sorted-matches
        (->> matches

             ;; We want a list of paths that is target-path-length + 1
             (map #(string-first-n-path-components (inc (count components)) %))
             distinct

             (sort))]

    ;; It's possible for the user to target a single record using "db show".
    ;; It that happens, we should just display the contents of that record instead of a
    ;; list of matched paths
    ;; Try to fetch the full recordname of the match now
    (if (= 1 (count sorted-matches))
      (printer/print-result-records
       [(dbu/record-by-name (first sorted-matches))])


      ;; If we have multiple record matches...
      ;; Just print the names
      (do
        (doseq [item sorted-matches]
          (u/print-line item))
        (u/print-line)
        (u/print-line (count sorted-matches) " matches")))))
