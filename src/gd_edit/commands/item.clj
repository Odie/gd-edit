(ns gd-edit.commands.item
  (:require [gd-edit
             [db-utils :as dbu]
             [globals :as globals]
             [inventory :as inventory]
             [printer :as printer]
             [structure-walk :as sw]
             [utils :as u]]
            [clojure.string :as str]
            [jansi-clj.core :refer :all]
            ))

;;------------------------------------------------------------------------------
;; Item utils
;;------------------------------------------------------------------------------
(defn- affix-record-get-name
  [affix-record]
  (affix-record "lootRandomizerName"))

(defn- item-or-affix-get-name
  [record]

  (or (affix-record-get-name record) (dbu/item-base-record-get-name record)))

(defn swap-first-two
  [vec]

  (->> (apply conj [] (second vec) (first vec) (drop 2 vec))))

(defn- baseitem-loottable-path
  [dbr]

  (as-> (str/split dbr #"/") $
        (nth $ 2)
        (str "records/items/loottables/" $ "/")))

(defn- all-loottable-records
  [db]

  (filter (fn [item]
            (str/starts-with?
             (:recordname item)
             ;; (baseitem-loottable-path baseitem-dbr-path)
             "records/items/loottables/"))
          db))

(defn- loottable-records-with-mentions
  [loottables mentioned-value]

  (filter (fn [item]
            (some
             #(= (val %) mentioned-value)
             item)
            )
          loottables))

(defn- baseitem-valid-affix-paths
  "Given the path of a base item, try to retrieve the valid affixes"
  [db db-index baseitem-dbr-path]

  ;; Retrieve records for all loot tables for that type of item (armor, shoulders, etc)
  (let [loottables (loottable-records-with-mentions (all-loottable-records db) baseitem-dbr-path)

        prefix-tables (reduce concat
                              '()
                              (apply conj
                                     (map #(u/collect-values-with-key-prefix % "prefixTableName") loottables)
                                     (map #(u/collect-values-with-key-prefix % "rarePrefixTableName") loottables)))

        suffix-tables (reduce concat
                              '()
                              (apply conj
                                     (map #(u/collect-values-with-key-prefix % "suffixTableName") loottables)
                                     (map #(u/collect-values-with-key-prefix % "rareSuffixTableName") loottables)))]

    {:prefix (->> prefix-tables
                  (map #(u/collect-values-with-key-prefix (get db-index %) "randomizerName"))
                  (reduce #(apply conj % %2) #{}))

     :suffix (->> suffix-tables
                  (map #(u/collect-values-with-key-prefix (get db-index %) "randomizerName"))
                  (reduce #(apply conj % %2) #{}))}))

;;------------------------------------------------------------------------------
;; Item generation
;;------------------------------------------------------------------------------
(defn compare-match-candidates
  "Expects 2 items in the form of [score candidate matched-name matched-records]. Return true or false indicating if c1 > c2."
  [c1 c2]

  (cond
    ;; If we're compare two items with the same score, prefer the one that matched the longer
    ;; token. This applies specifically when we found more than one item that perfectly
    ;; matched the partial name of an item.
    (= (first c1) (first c2))
    (> (count (second c1)) (count (second c2)))

    ;; If the scores are clearly different, then just compare the score
    (> (first c1) (first c2))
    true

    :else
    false))

(defn prefer-material-over-blueprint
  [coll]

  (if (and (> (count coll) 1)

           ;; Do the items have the exact same name?
           (= (dbu/item-base-record-get-name (first coll)) (dbu/item-base-record-get-name (second coll)))

           ;; Is the preferred one a blueprint?
           (u/ci-match (:recordname (first coll)) "/items/crafting/blueprints/")
           (u/ci-match (:recordname (second coll)) "/items/crafting/materials/"))
    (swap-first-two coll)
    coll))

(defn build-item-name-idx
  [db]
  (let [item-records (filter (fn [record]
                               (and
                                (some #(str/starts-with? (:recordname record) %1)
                                      #{"records/items/"})
                                (not (str/starts-with? (:recordname record) "records/items/lootaffixes"))
                                (dbu/record-has-display-name? record)
                                ))

                             db)

        item-name-idx (->> (group-by #(dbu/item-base-record-get-name %1) item-records)
                           (map (fn [[k v]] [k (prefer-material-over-blueprint v)]))
                           (into {})
                           )]
    item-name-idx
    ))

(defn name-idx-best-matches
  [name-idx target-name]

  (->> name-idx

       ;; Score and sort the item name index
       ;; First, we rank by the name's overall similiarity
       ;; This should help to filter out items that are not at all similar
       (map (fn [[item-name item-record]]
              [(u/string-similarity (str/lower-case item-name) (str/lower-case target-name)) item-name item-record]))
       (sort-by first >)
       ))

(defn idx-best-match
  "Take the index and an array of 'tokenized' item name, try to find the best match.
  Returns [score candidate matched-name matched-records]]"
  [idx item-name-tokens]

  (let [;; Build a list of candidates to attempt matching against know prefixes
        candidates (->> item-name-tokens
                        (reduce (fn [results token]
                                  (conj results (conj (last results) token)))

                                [[]]
                                )
                        (drop 1))

        ;; Try to locate a best match by ranking all candidates
        best-match (->> candidates
                        (map (fn [candidate]
                               ;; For each candidate, return a pair [best-match, candidate]
                               (let [[score matched-name matched-records] (->> (clojure.string/join " " candidate)
                                                                               (name-idx-best-matches idx)
                                                                               (first))]
                                 [score candidate matched-name matched-records])))
                        (sort compare-match-candidates)
                        (first))]

    ;; Does the best match seem "good enough"?
    (if (and (not (nil? (get best-match 0)))
             (> (nth best-match 0) 0.85))
      best-match
      nil)))

(defn analyze-multipart-item-name
  [item-name-idx prefix-name-idx suffix-name-idx item-name]

  (let [tokens (clojure.string/split item-name #" ")
        tokens-cursor tokens

        ;; See if we can match part of the item name against the prefix index
        prefix-best-match (idx-best-match prefix-name-idx tokens-cursor)
        prefix-record (if (nil? prefix-best-match)
                        nil
                        (do
                          (get-in prefix-best-match [3 0])))

        ;; Advance the tokens cursor if possible
        tokens-cursor (if (not (nil? prefix-best-match))
                        (drop (count (nth prefix-best-match 1)) tokens-cursor)
                        tokens-cursor)

        ;; See if we can match part of the item name against the item name index
        base-best-match (idx-best-match item-name-idx tokens-cursor)
        base-record (if (nil? base-best-match)
                      nil
                      (do
                        (get-in base-best-match [3 0])))

        ;; Advance the tokens cursor if possible
        tokens-cursor (if (not (nil? base-best-match))
                        (drop (count (nth base-best-match 1)) tokens-cursor)
                        tokens-cursor
                        )

        ;; See if we can match part of the item name against the suffix name index
        suffix-best-match (idx-best-match suffix-name-idx tokens-cursor)
        suffix-record (if (nil? suffix-best-match)
                      nil
                      (do
                        (get-in suffix-best-match [3 0])))

        ;; Advance the tokens cursor if possible
        tokens-cursor (if (not (nil? suffix-best-match))
                        (drop (count (nth suffix-best-match 1)) tokens-cursor)
                        tokens-cursor)
        ]

    {:base base-record
     :prefix prefix-record
     :suffix suffix-record
     :remaining-tokens tokens-cursor
     }))

(defn- item-def->item
  [def]

  {:basename       (or (get-in def [:base :recordname]) "")
   :prefix-name    (or (get-in def [:prefix :recordname]) "")
   :suffix-name    (or (get-in def [:suffix :recordname]) "")
   :modifier-name  ""
   :transmute-name ""
   :seed           (rand-int Integer/MAX_VALUE)

   :relic-name     ""
   :relic-bonus    ""
   :relic-completion-level 0
   :relic-seed     0

   :augment-name   ""
   :unknown        0
   :augment-seed   0

   :var1        0
   :stack-count 1})

(defn analyze-item-name
  [item-name-idx prefix-name-idx suffix-name-idx target-name]

  (let [tokens (clojure.string/split target-name #" ")
        tokens-cursor tokens

        ;; Try to match against the whole name directly
        whole-item-match {:base (get-in (idx-best-match item-name-idx tokens-cursor) [3 0])}

        ;; Try to break down the item into multiple part components
        multi-part-match (analyze-multipart-item-name item-name-idx prefix-name-idx suffix-name-idx target-name)

        ;; Of the two results, figure out which one matches the input name more closely
        ;; Return that item
        match (->> [whole-item-match multi-part-match]
                   (sort-by #(u/string-similarity
                              (str/lower-case target-name)
                              (str/lower-case (or
                                               (dbu/item-name (item-def->item %1) @globals/db)
                                               "")))
                            >)
                   (first))
        ]

     match))

(defn name-idx-highest-level-by-name
  [idx name level-cap]

  ;; Grab the records referenced by the name
  (let [records (idx name)]

    ;; Locate the highest level item that does not exceed the level-cap
    (->> records
         (filter #(>= level-cap (or (%1 "levelRequirement") (%1 "itemLevel") 0)))
         (sort-by #(or (%1 "levelRequirement") (%1 "itemLevel") 0) >)
         (first))))

(defn item-materia-fill-completion-level
  [item]

  (if (dbu/item-is-materia? item)
    (assoc item :relic-completion-level 4)
    item))

(defn build-affix-idx
  [coll type]

  (let [recordname-prefix (if (= type :prefix)
                        "records/items/lootaffixes/prefix/"
                        "records/items/lootaffixes/suffix/")

        affix-records (->> coll
                           (filter #(str/starts-with? (:recordname %1) recordname-prefix))
                           (filter #(-> (:recordname %1)
                                        (str/split #"/")
                                        (count)
                                        (= 5))))]

    (group-by #(%1 "lootRandomizerName") affix-records)))

(defn- analysis->item
  [analysis db item-name-idx prefix-name-idx suffix-name-idx level-cap]

  (let [;; Now that we know what the item is composed of, try to bring up the strength/level of each part
        ;; as the level cap allows
        ;; For example, there are 16 different suffixes all named "of Potency". Which one do we choose?
        ;; We choose highest level one that is less or equal to the level cap
        results (->> (select-keys analysis [:base :prefix :suffix])
                     (map (fn [[key record]]
                            [key

                             (if (nil? record)
                               nil
                               (name-idx-highest-level-by-name
                                (cond
                                  (= key :base)
                                  item-name-idx
                                  (= key :prefix)
                                  prefix-name-idx
                                  (= key :suffix)
                                  suffix-name-idx)
                                (item-or-affix-get-name record)
                                level-cap))])
                          )
                     (into {}))]

    ;; An item must have a basename, which should refer to a item record
    ;; If we could not find one that satisfies the target-name, then return nothing.
    (if (nil? (:base results))
      nil

      ;; Otherwise, create a hashmap that represents the item
      (->> (item-def->item results)
           (item-materia-fill-completion-level)))))

(defn- non-nil-value-count
  [m]

  (reduce-kv (fn [accum k v]
                   (if-not (nil? v)
                     (+ accum 1)
                     accum
                     ))
             0
             m))

(defn- analysis-better-match
  [a1 a2]

  (let [score1 (non-nil-value-count a1)
        score2 (non-nil-value-count a2)]
    (if (> score2 score1)
      a2
      a1)))

(defn construct-item
  [target-name db db-index level-cap]

  ;; Build a list of base item names with their quality name
  ;; The index takes the form of: item-name => [item-records]
  ;; Multiple records may have the same name, but differ in strength
  (let [item-name-idx (build-item-name-idx db)

        ;; Try to decompose the complete item name into its prefix, base, and suffix, using
        ;; all known affixes
        prefix-all (build-affix-idx db :prefix)
        suffix-all (build-affix-idx db :suffix)
        analysis-all (analyze-item-name item-name-idx
                                        prefix-all
                                        suffix-all
                                        target-name)

        ;; Now that we have a good idea of the basename of the item,
        ;; try to narrow down to "legal" prefixes only.
        legal-affixes (into {}
                            (map (fn [[k affix-set]]
                                   [k (map #(get db-index %)
                                           affix-set)])
                                 (baseitem-valid-affix-paths @globals/db @globals/db-index (:recordname (:base analysis-all)))))

        prefix-narrow (build-affix-idx (legal-affixes :prefix) :prefix)
        suffix-narrow (build-affix-idx (legal-affixes :suffix) :suffix)
        analysis-narrow (analyze-item-name item-name-idx
                                           prefix-narrow
                                           suffix-narrow
                                           target-name)

        analysis-final (analysis-better-match analysis-narrow analysis-all)
        [prefix-final suffix-final] (if (= analysis-final analysis-narrow)
                                      [prefix-narrow suffix-narrow]
                                      [prefix-all suffix-all])
        ]


    (analysis->item analysis-final db item-name-idx
                    prefix-final
                    suffix-final
                    level-cap)
    )
  )

(defn- path-is-item?
  [character path]

  (contains? (get-in @character path) :basename))

(defn- path-is-inventory?
  [character path]

  (and (= (count path) 3)
       (= (first path) :inventory-sacks)
       (= (last path) :inventory-items)))

(defn place-item-in-inventory!
  [character val-path item]

  (cond
    ;; If the caller indicated an existing item,
    ;; replace it with the new given item.
    (path-is-item? character val-path)
    (do
      (swap! character assoc-in val-path
             (merge (get-in @character val-path) item))
      true)

    ;; Did the call ask to have an item added to an inventory/sack?
    (path-is-inventory? character val-path)
    (if-let [coord (inventory/fit-new-item (second val-path)
                                           (get-in @character val-path)
                                           item)]
      (do
        (swap! character update-in val-path conj (merge item coord))
        true)

      ;; TODO Implement better error handling pattern!
      false)

    :else
    (throw (Throwable. "Unhandled case"))))

;;------------------------------------------------------------------------------
;; Command handlers
;;------------------------------------------------------------------------------
(defn set-item-handler
  [[input [path target-name level-cap-str]]]

  (let [path-keys (str/split path #"/")
        result (sw/walk @gd-edit.globals/character path-keys)
        {:keys [status found-item actual-path]} result

        level-cap (if-not (nil? level-cap-str)
                    (Integer/parseInt level-cap-str)
                    (:character-level @globals/character))]

    (cond
      (= status :not-found)
      (println "The path does not specify an item")

      (= status :too-many-matches)
      (sw/print-ambiguous-walk-result result)


      :else
      ;; Try to construct the requested item
      (let [item (construct-item target-name @globals/db @globals/db-index level-cap)]
        ;; If construction was not successful, inform the user and abort
        (cond
          (nil? item)
          (println "Sorry, the item could not be constructed")

          (not (> (u/string-similarity (str/lower-case target-name) (str/lower-case (dbu/item-name item @globals/db))) 0.8))
          (do
            (printer/show-item item)
            (println "Sorry, the item generated doesn't look like the item you asked for.")
            (println (red "Item not altered")))

          ;; Otherwise, put the item into the character sheet
          :else
          (if-not (place-item-in-inventory! globals/character actual-path item)
            (println "Sorry, there is no room to fit the item.")
            (printer/show-item item)))))))

#_(set-item-handler  [nil ["inv/1/items/0" "legion warhammer of valor" "64"]])
#_(set-item-handler  [nil ["equipment/3" "stonehide Dreeg-Sect Legguards of the boar" "75"]])
#_(gd-edit.command-handlers/show-handler [nil ["equipment/3"]])

(comment

  (baseitem-valid-affix-paths @globals/db @globals/db-index "records/items/gearshoulders/b020a_shoulder.dbr")

  (count
   (all-loottable-records @globals/db))

  (loottable-records-with-mentions (all-loottable-records @globals/db) "records/items/faction/weapons/blunt1h/f002a_blunt.dbr")

  )
