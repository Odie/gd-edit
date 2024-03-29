(ns gd-edit.structure-walk
  (:require [gd-edit.utils :as u]
            [gd-edit.db-utils :as dbu]
            [clojure.string :as str]))

(defn- str-without-dash
  [str]
  (str/replace str "-" ""))

(defn- coerce-to-int
  [input]

  (cond
    (number? input)
    (int input)

    (string? input)
    (Integer/parseInt input)

    :else
    (throw (Throwable. "Can't deal with input"))))

(defn- nav-into
  [coll key]

  (cond
    ;; If we're looking at a sequential collection, try to interpret key as an index
    (sequential? coll)
    (nth coll (coerce-to-int key))

    (associative? coll)
    (get coll key)

    :else
    (throw (Throwable. "Can't deal with coll with type: " (type coll)))))

(defn- partially-match-key
  "Given a map and a partial key, return a list of pairs that partially matches the key"
  [m search-target]

  (let [haystack (filter dbu/without-meta-fields m)

        ;; Locate all partial matches
        partial-matches (filter (fn [[key _]]
                                  (let [k-str (u/keyword->str key)]
                                    (or (u/ci-match k-str search-target)
                                        (u/ci-match (str-without-dash k-str) search-target))))
                                haystack)

        ;; Are any of the partial matches actual exact matches?
        ;; If we don't look for
        exact-match (filter (fn [[key _]]
                              (let [k-str (u/keyword->str key)]
                                (or (and (u/ci-match k-str search-target)
                                         (= (count k-str) (count search-target)))
                                    (and (u/ci-match (str-without-dash k-str) search-target)
                                         (= (count (str-without-dash k-str)) (count search-target))))))
                            partial-matches)]

    ;; If we can find an exact match, use that.
    ;; Otherwise, return the partial matches
    (if (= (count exact-match) 1)
      exact-match
      partial-matches)))

(defn walk
  [m ks-all]

  (letfn [;; We want to return a slightly more complex set of data to the caller.
          ;; This helper function helps us construct the return map so we don't have
          ;; to sprinkle this everywhere.
          (return-result [result end-ks actual-path & others]
            (merge {;; Report the result the caller said to report
                    :status result

                    ;; Calculate the longest path traversed
                    :longest-path (take (- (count ks-all) (count end-ks)) ks-all)
                    :actual-path actual-path}

                   (first others)))]

    ;; For each item in the specified ks
    ;; Try to iterate a level deeper using the next key
    (loop [cursor m
           ks ks-all
           actual-path []]

      ;; (u/print-line "ks" ks)
      ;; (u/print-line "actual-path" actual-path)
      ;; Which key are we trying to navigate to?
      (let [k (first ks)]
        (cond
          ;; Did we exhaust all the keys?
          ;; If so, we're done navigating into the hierarchy.
          ;; The cursor should be pointing at the item the user wants
          (nil? k)
          (do
            ;; (u/print-line "nil k")
            (return-result :found ks actual-path {:found-item cursor}))

          ;; Did we try navigating into a location that doesn't exist?
          (nil? cursor)
          (do
            ;; (u/print-line "nil cursor")
            (return-result :not-found ks actual-path))

          ;; If we have a sequential collection, just try to navigate into
          ;; the collection with the key
          (sequential? cursor)
          (do
            ;; (u/print-line "br sequential")
            (recur (nav-into cursor k) (rest ks) (conj actual-path (coerce-to-int k))))

          ;; If we're looking at an associative collection, then we want to
          ;; perform partial matching on the current key
          (associative? cursor)
          (let [matches (partially-match-key cursor k)]
            (cond
              ;; If we can't get a match at all, we cannot navigate to the key
              (= (count matches) 0)
              (do
                ;; (u/print-line "br1")
                (return-result :not-found (rest ks) actual-path))

              ;; If we have more than one match, tell the caller we cannot
              ;; resolve this ambiguity.
              (> (count matches) 1)
              (do
                ;; (u/print-line "br2")
                (return-result :too-many-matches (rest ks) actual-path {:ambiguous-matches matches}))

              :else
              (do
                ;; (u/print-line "br3")
                (let [[matched-key matched-value :as matches] (first matches)]
                  ;; (u/print-line "matches" matches)
                  (recur matched-value (rest ks) (conj actual-path matched-key))))))

          :else
          (return-result :cannot-traverse ks actual-path))))))

(defn print-ambiguous-walk-result
  [result]

  (u/print-line (format "Cannot traverse path because \"%s\""
                   (str/join "/" (:longest-path result)))
           "matches more than one item:")

  (doseq [ambiguous-item (->> (:ambiguous-matches result)
                              (keys)
                              (map u/keyword->str)
                              (sort))]
    (u/print-indent 1)
    (u/print-line ambiguous-item)))
