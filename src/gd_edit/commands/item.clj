(ns gd-edit.commands.item
  (:require [gd-edit
             [db-utils :as dbu]
             [globals :as globals]
             [inventory :as inventory]
             [printer :as printer]
             [stack :as stack]
             [structure-walk :as sw]
             [utils :as u]]
            [gd-edit.commands.help :as help]
            [clojure.string :as str]
            [jansi-clj.core :refer [red green yellow]]))

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

  (apply conj [] (second vec) (first vec) (drop 2 vec)))

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
             "records/items/loottables/"))
          db))

(defn- loottable-records-with-mentions
  [loottables mentioned-value]

  (filter (fn [item]
            (some
             #(= (val %) mentioned-value)
             item))
          loottables))

(defn- baseitem-valid-affix-paths
  "Given the path of a base item, try to retrieve the valid affixes"
  [db db-index baseitem-dbr-path]

  ;; Retrieve records for all loot tables for that type of item (armor, shoulders, etc)
  (let [loottables (loottable-records-with-mentions (all-loottable-records db) baseitem-dbr-path)
        prefix (->> (concat
                     (mapcat #(u/collect-values-with-key-prefix % "prefixTableName") loottables)
                     (mapcat #(u/collect-values-with-key-prefix % "rarePrefixTableName") loottables))
                    (map #(u/collect-values-with-key-prefix (get db-index %) "randomizerName"))
                    (apply concat)
                    (into #{}))
        suffix (->> (concat
                     (mapcat #(u/collect-values-with-key-prefix % "suffixTableName") loottables)
                     (mapcat #(u/collect-values-with-key-prefix % "rareSuffixTableName") loottables))
                    (map #(u/collect-values-with-key-prefix (get db-index %) "randomizerName"))
                    (apply concat)
                    (into #{}))]
    {:prefix prefix
     :suffix suffix}))

;;------------------------------------------------------------------------------
;; Item generation
;;------------------------------------------------------------------------------
(defn compare-match-candidates
  "Expects 2 items in the form of [score candidate matched-name matched-records].
  Return true or false indicating if c1 > c2."
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

(def filter-db-records-for-items-with-display-name
  (filter #(let [class (dbu/record-class %)]
             (and
              (or (u/ci-match class "item")
                  (u/ci-match class "armor")
                  (u/ci-match class "weapon")
                  (u/ci-match class "oneshot")
                  (u/ci-match class "formula")
                  (u/ci-match class "relic"))
              (not
               (or (u/ci-match class "FixedItem")
                   (u/ci-match class "Table")))
              (dbu/record-has-display-name? %)))))


(defn build-item-name-idx
  [db]
  (let [item-records (into [] filter-db-records-for-items-with-display-name db)

        item-name-idx (->> (group-by #(dbu/item-base-record-get-name %1) item-records)
                           (map (fn [[k v]] [k (prefer-material-over-blueprint v)]))
                           (into {}))]
    item-name-idx))


(defn name-idx-best-matches
  [name-idx target-name]

  (->> name-idx

       (filter (fn [[item-name item-record]]
                 (not (nil? item-name))))

       ;; Score and sort the item name index
       ;; First, we rank by the name's overall similarity
       ;; This should help to filter out items that are not at all similar
       (pmap (fn [[item-name item-record]]
               [(u/string-similarity (str/lower-case item-name) (str/lower-case target-name)) item-name item-record]))

       (sort-by first >)))

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
                               (let [[score matched-name matched-records] (->> (str/join " " candidate)
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

  (let [tokens (str/split item-name #" ")
        tokens-cursor tokens

        ;; See if we can match part of the item name against the prefix index
        prefix-best-match (idx-best-match prefix-name-idx tokens-cursor)
        prefix-record (if (nil? prefix-best-match)
                        nil
                        (get-in prefix-best-match [3 0]))

        ;; Advance the tokens cursor if possible
        tokens-cursor (if (not (nil? prefix-best-match))
                        (drop (count (nth prefix-best-match 1)) tokens-cursor)
                        tokens-cursor)

        ;; See if we can match part of the item name against the item name index
        base-best-match (idx-best-match item-name-idx tokens-cursor)
        base-record (if (nil? base-best-match)
                      nil
                      (get-in base-best-match [3 0]))

        ;; Advance the tokens cursor if possible
        tokens-cursor (if (not (nil? base-best-match))
                        (drop (count (nth base-best-match 1)) tokens-cursor)
                        tokens-cursor
                        )

        ;; See if we can match part of the item name against the suffix name index
        suffix-best-match (idx-best-match suffix-name-idx tokens-cursor)
        suffix-record (if (nil? suffix-best-match)
                      nil
                      (get-in suffix-best-match [3 0]))

        ;; Advance the tokens cursor if possible
        tokens-cursor (if (not (nil? suffix-best-match))
                        (drop (count (nth suffix-best-match 1)) tokens-cursor)
                        tokens-cursor)]

    {:base base-record
     :prefix prefix-record
     :suffix suffix-record
     :remaining-tokens tokens-cursor}))

(defn blank-item
  []
  {:basename       ""
   :prefix-name    ""
   :suffix-name    ""
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

(defn- item-def->item
  [def]

  (merge
   (blank-item)
   {:basename       (or (get-in def [:base :recordname]) "")
    :prefix-name    (or (get-in def [:prefix :recordname]) "")
    :suffix-name    (or (get-in def [:suffix :recordname]) "")}))

(defn analyze-item-name
  [item-name-idx {prefix-name-idx :prefix suffix-name-idx :suffix} target-name]

  (let [tokens (str/split target-name #" ")]
    (u/plet [;; Try to match against the whole name directly
             whole-item-match {:base (get-in (idx-best-match item-name-idx tokens) [3 0])}

             ;; Try to break down the item into multiple part components
             multi-part-match (analyze-multipart-item-name item-name-idx prefix-name-idx suffix-name-idx target-name)]

            ;; Of the two results, figure out which one matches the input name more closely
            ;; Return that item
            (->> [whole-item-match multi-part-match]
                 (sort-by #(u/string-similarity
                            (str/lower-case target-name)
                            (str/lower-case (or
                                             (dbu/item-name (item-def->item %1) (dbu/db-and-index))
                                             "")))
                          >)
                 (first)))))

(defn name-idx-highest-level-by-name
  [idx name level-cap]

  ;; Grab the records referenced by the name
  (let [records (idx name)]

    ;; Locate the highest level item that does not exceed the level-cap
    (cond->> records
      (some? level-cap)
      (filter #(>= level-cap (or (%1 "levelRequirement") 0)))

      :then
      (sort-by #(or (%1 "levelRequirement") (%1 "itemLevel") 0) >)

      :then
      (first))))

(defn item-materia-fill-completion-level
  [item]

  (if (dbu/item-is-materia? item)
    (assoc item :relic-completion-level 4)
    item))

(defn path-component-count
  [path]

  (-> path
      (str/split #"/")
      (count)))


(defn group-by-loot-name
  [affix-records]
  (group-by #(%1 "lootRandomizerName") affix-records))


(defn- group-affixes-by-loot-name
  [affixes]

  (-> affixes
      (update :prefix group-by-loot-name)
      (update :suffix group-by-loot-name)))

(defn filter-for-record-of-class
  [class-name]
  (filter #(= (dbu/record-class %) class-name)))

(def filter-for-affixes
  (filter-for-record-of-class "LootRandomizer"))

(defn categorize-prefix-or-suffix
  [record]

  (let [recordname (:recordname record)]
    (cond
      (u/ci-match recordname "prefix")
      :prefix

      (u/ci-match recordname "suffix")
      :suffix

      :else
      :unknown)))

(defn reshape-records-as-affixes
  "Given some records, return a {:prefix [...] :suffix [...]} map, where
  all prefix records are stored under :prefix
    and
  all suffix records are stored under :suffix"
  [records]
  (select-keys (->> records
                    (into #{} filter-for-affixes)
                    (group-by categorize-prefix-or-suffix))
               [:prefix :suffix]))

(defn- analysis->item
  [analysis item-name-idx {prefix-name-idx :prefix suffix-name-idx :suffix} level-cap]

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
                                level-cap))]))

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

  (reduce-kv (fn [accum _ v]
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
  ([target-name level-cap]
   (construct-item target-name (dbu/db) (dbu/db-recordname-index) level-cap))

  ([target-name db db-index level-cap]

   ;; Build a list of base item names with their quality name
   ;; The index takes the form of: item-name => [item-records]
   ;; Multiple records may have the same name, but differ in strength
   (let [item-name-idx (build-item-name-idx db)

         ;; Try to decompose the complete item name into its prefix, base, and suffix, using
         ;; all known affixes
         affixes-all (->> db
                          (reshape-records-as-affixes)
                          (group-affixes-by-loot-name))
         analysis-all (analyze-item-name item-name-idx
                                         affixes-all
                                         target-name)

         ;; Now that we have a good idea of the basename of the item,
         ;; try to narrow down to "legal" prefixes only.
         legal-affixes (->> (:recordname (:base analysis-all))
                            (baseitem-valid-affix-paths (dbu/db) (dbu/db-recordname-index) )


                            (map (fn [[k affix-set]]
                                   [k (->> affix-set
                                           (map #(db-index %))
                                           (filter some?))]))
                            (into {}))

         affixes-legal (group-affixes-by-loot-name legal-affixes)
         analysis-legal (analyze-item-name item-name-idx
                                           affixes-legal
                                           target-name)

         analysis-final (analysis-better-match analysis-legal analysis-all)
         affixes-final (if (= analysis-final analysis-legal)
                         affixes-legal
                         affixes-all)]
     (analysis->item analysis-final
                     item-name-idx
                     affixes-final
                     level-cap))))

(defn- path-is-item?
  [character path]

  (contains? (get-in character path) :basename))

(defn path-is-inventory?
  [path]

  (or (and (= (first path) :inventory-sacks)
           (= (last path) :inventory-items))
      (and (= (first path) :transfer-stashes)
           (= (last path) :inventory-items))))

(defn path-is-shrine-list?
  [path]
  (and (= (first path) :shrines)
       (= (count path) 2)))

(defn path-is-rift-gate-list?
  [path]
  (and (= (first path) :teleporter-points)
       (= (count path) 2)))

(defn place-item-in-inventory!
  [character val-path item]

  (cond
    ;; If the caller indicated an existing item,
    ;; replace it with the new given item.
    (path-is-item? character val-path)
    (do
      (swap! character assoc-in val-path
             (merge (get-in @character val-path) item))
      val-path)

    ;; Did the call ask to have an item added to an inventory/sack?
    (path-is-inventory? val-path)
    (if-let [coord (inventory/fit-new-item val-path item)]
      (do
        (swap! character update-in val-path conj (merge item coord))
        (conj val-path (dec (count (get-in @character val-path)))))

      ;; TODO Implement better error handling pattern!
      false)

    :else
    (throw (Throwable. "Unhandled case"))))

(defn place-item-in-inventory
  "Tries the place the specified `item` at the `val-path` of the character.
  Answers with [character item-path]"
  [character val-path item]

  (cond
    ;; If the caller indicated an existing item,
    ;; replace it with the new given item.
    (path-is-item? character val-path)
    [(assoc-in character val-path
               (merge (get-in character val-path) item))
     val-path]

    ;; Did the call ask to have an item added to an inventory/sack?
    (path-is-inventory? val-path)
    (if-let [coord (inventory/fit-new-item val-path item)]
      [(update-in character val-path conj (merge item coord))
       (conj val-path (dec (count (get-in @character val-path))))]

      ;; TODO Implement better error handling pattern!
      [character nil])

    :else
    (throw (Throwable. (str "Unhandled case trying to place an item at: " (or val-path "nil"))))))

(defn item-names-similiar
  "Test an item against some target name and return if they are similar"
  [target-item-name test-item-name]

  (> (u/string-similarity (str/lower-case target-item-name)
                          (str/lower-case test-item-name)) 0.8))

;;------------------------------------------------------------------------------
;; Command handlers
;;------------------------------------------------------------------------------
(defn data-at-fuzzy-path
  [character path-str]
  (sw/walk character (str/split path-str #"/")))

(defn data-located-or-print-error
  [walk-result]
  (let [{:keys [status]} walk-result]
    (cond
      (= status :not-found)
      (u/print-line "The path does not specify an item")

      (= status :too-many-matches)
      (sw/print-ambiguous-walk-result walk-result)

      :else
      walk-result)))


(defn set-item-handler
  [[input [path target-name level-cap-str]]]

  ;; Do some sanity checks before we try creating and placing an item
  (if-let [result (->> (data-at-fuzzy-path @globals/character path)
                       (data-located-or-print-error))]
    (let [{:keys [actual-path]} result
          level-cap (when (some? level-cap-str)
                      (Integer/parseInt level-cap-str))

          ;; Try to construct the requested item
          item (construct-item target-name (dbu/db) (dbu/db-recordname-index) level-cap)]

      ;; If construction was not successful, inform the user and abort
      (cond
        (nil? item)
        (u/print-line "Sorry, the item could not be constructed")

        (not (item-names-similiar target-name (dbu/item-name item)))
        (do
          (printer/show-item item)
          (u/print-line "Sorry, the item generated doesn't look like the item you asked for.")
          (u/print-line (red "Item not altered")))

        ;; Otherwise, put the item into the character sheet
        :else
        (if-let [placed-path (place-item-in-inventory! globals/character actual-path item)]
          (do
            (u/print-line "Item placed in" (yellow (printer/displayable-path placed-path)))
            (u/print-line)
            (printer/show-item item))
          (u/print-line "Sorry, there is" (red "no room") "to fit the item."))))))


(defn granted-skill-name
  [record]
  (-> record
      (get "itemSkillName")
      dbu/record-by-name
      dbu/skill-name-from-record))

(defn affix-name-idx-by-type
  [affix-type]
  (let [affixes (->> (dbu/db)
                     (reshape-records-as-affixes)
                     affix-type)
        affixes-by-granted-skill-name (->> affixes
                                           (map #(vector (granted-skill-name %) %))
                                           (remove #(nil? (first %)))
                                           (into {}))
        affixes-by-name (->> affixes
                             (map #(vector (affix-record-get-name %) %))
                             (remove #(nil? (first %)))
                             (into {}))]
    (merge affixes-by-name affixes-by-granted-skill-name)))

(defn set-affix-handler
  [[input [path target-name]]]

  ;; Do some sanity checks before we try creating and placing an item
  (if-let [result (->> (data-at-fuzzy-path @globals/character path)
                       (data-located-or-print-error))]
    (let [{:keys [actual-path]} result
          affix-type (cond
                       (= (last actual-path) :prefix-name)
                       :prefix
                       (= (last actual-path) :suffix-name)
                       :suffix)

          affixes-idx (affix-name-idx-by-type affix-type)
          best-match (first (u/rank-by-similarity target-name first affixes-idx))
          matched-affix-name (get-in best-match [:item 0])
          affix (get-in best-match [:item 1])]

      (cond
        (nil? affix)
        (u/print-line (u/fmt "Sorry, the affix \"#{target-name}\" could not be found"))

        (not (> (u/string-similarity (str/lower-case target-name) matched-affix-name) 0.75))
        (do
          (u/print-line (u/fmt "Sorry, the closest affix match \"#{matched-affix-name}\" doesn't look like the affix you asked for."))
          (u/print-line (red "Item not altered")))

        :else
        (do
          (swap! globals/character update-in actual-path (constantly (:recordname affix)))
          (u/print-line (green "Item altered"))
          (let [path-to-item (drop-last actual-path)]
            (printer/print-object (get-in @globals/character path-to-item) path-to-item)))))))

(defn batch-items!
  [item inv-path]
  (loop [item item]
    (if-let [placed-item (place-item-in-inventory! globals/character inv-path item)]
      (recur (assoc item :seed (rand-int Integer/MAX_VALUE))))))

(defn batch-item-handler
  [[input [path target-name level-cap-str]]]

  ;; Do some sanity checks before we try creating and placing an item
  (if-let [result (->> (data-at-fuzzy-path @globals/character path)
                       (data-located-or-print-error))]
    (let [{:keys [actual-path]} result
          level-cap (when (some? level-cap-str)
                      (Integer/parseInt level-cap-str))

          ;; Try to construct the requested item
          item (construct-item target-name (dbu/db) (dbu/db-recordname-index) level-cap)]

      ;; If construction was not successful, inform the user and abort
      (cond
        (nil? item)
        (u/print-line "Sorry, the item could not be constructed")

        (not (> (u/string-similarity (str/lower-case target-name) (str/lower-case (dbu/item-name item (dbu/db-and-index)))) 0.8))
        (do
          (printer/show-item item)
          (u/print-line "Sorry, the item generated doesn't look like the item you asked for.")
          (u/print-line (red "Item not altered")))

        ;; Otherwise, put the item into the character sheet
        :else
        (let [before (get-in @globals/character actual-path)
              _ (batch-items! item actual-path)
              after (get-in @globals/character actual-path)]
          (printer/show-item item)
          (u/print-line)
          (u/print-line "Placed" (yellow (- (count after) (count before))) "items into" (u/keywords->path actual-path)))))))


(defn- swap-variant-screen
  [item-path target-field variants]

  (let [choices (->> (for [[idx variant] (->> variants
                                              (sort-by :recordname)
                                              (map-indexed vector))]
                       [;; choice label
                        (str (inc idx))

                        ;; choice text
                        ;; we'll print the unique fields of each variant
                        (with-out-str
                          (u/print-line (:recordname variant))
                          (printer/print-map (->> variant
                                                  (remove #(= :recordname (key %))))
                                             :skip-item-count true))

                        ;; choice action
                        (fn[]
                          ;; We're only know how to change the basename based on user choice for now
                          (let [path-to-target-field (conj item-path target-field)]
                            (swap! globals/character update-in path-to-target-field
                                   (constantly (:recordname variant)))

                            ;; Exit the menu
                            (stack/pop! globals/menu-stack)))])
                     (into []))
        choices (conj choices ["a" "abort" (fn[] (stack/pop! globals/menu-stack))])]

    {:display-fn
     (fn []
       (printer/displayable-path item-path)
       (u/print-line (format "Pick a variant for the %s of %s"
                        (u/keyword->str target-field)
                        (dbu/item-name
                         (get-in @globals/character item-path) (dbu/db-and-index))))
       (u/newline-))

     :choice-map choices}))

(defn swap-variant-screen! [item-path target-field variants] (stack/push! globals/menu-stack
                                                                          (swap-variant-screen item-path target-field variants)))


(defn- swap-variant-print-usage
  []
  (u/print-line (->> (help/get-help-item "swap-variant")
                (help/detail-help-text))))

(defn swap-variant-handler
  [[input [path target-field-name]]]

  (if (<= (count path) 1)
    (swap-variant-print-usage)

    (if-let [result (->> (data-at-fuzzy-path @globals/character path)
                         (data-located-or-print-error))]
      (let [{:keys [found-item actual-path]} result
            target-field-name (or target-field-name
                                  "basename")
            target-field (get {"basename" :basename
                               "prefix" :prefix-name
                               "suffix" :suffix-name}
                              target-field-name)
            variants (->> (dbu/record-variants (dbu/db) (target-field found-item))
                          (map set))]
        (cond
          (not (contains? found-item target-field))
          (u/print-line (format "Sorry, can't find '%s' on the item at '%'" target-field-name (printer/displayable-path actual-path)))

          (<= (count variants) 1)
          (u/print-line "Sorry, can't find any" target-field-name "variants for" (yellow (dbu/item-name found-item (dbu/db-and-index))))

          :else
          (swap-variant-screen! actual-path
                                target-field
                                (dbu/unique-item-record-fields variants)))))))

(comment

  (swap-variant-handler [nil ["weapon-sets/1/items/0" "suffix"]])

  (baseitem-valid-affix-paths (dbu/db) (dbu/db-recordname-index)  "records/items/gearshoulders/b020a_shoulder.dbr")

  (count
   (all-loottable-records (dbu/db)))

  (loottable-records-with-mentions (all-loottable-records (dbu/db)) "records/items/faction/weapons/blunt1h/f002a_blunt.dbr")

  (set-item-handler  [nil ["inv/1/items" "Oleron's Wrath"]])
  (set-item-handler  [nil ["inv/1/items" "mythical stormheart"]])

  (set-item-handler  [nil ["inv/1/items" "stonehide dreeg-sect legguards of the boar"]])


  ;; Item with quality tag
  (assert (= (select-keys (construct-item "mythical stormheart" (dbu/db) (dbu/db-and-index) nil) [:basename])
             {:basename "records/items/upgraded/gearweapons/swords1h/d010_sword.dbr"}))

  ;; Relic generation
  (assert (= (select-keys (construct-item "Oleron's Wrath" (dbu/db) (dbu/db-and-index) nil) [:basename])
             {:basename "records/items/gearrelic/d019_relic.dbr"}))

  ;; Item with suffix
  (assert (= (select-keys (construct-item "legion warhammer of valor" (dbu/db) (dbu/db-and-index) nil) [:basename :suffix-name])
             {:basename "records/items/faction/weapons/blunt1h/f002b_blunt.dbr"
              :suffix-name "records/items/lootaffixes/suffix/b_sh002_g.dbr"}))

  ;; Item with prefix and suffix
  (assert (= (select-keys (construct-item "stonehide Dreeg-Sect Legguards of the boar" (dbu/db) (dbu/db-and-index) nil) [:basename :prefix-name :suffix-name])

             {:basename "records/items/gearlegs/b001e_legs.dbr"
              :prefix-name "records/items/lootaffixes/prefix/b_ar028_ar_f.dbr"
              :suffix-name "records/items/lootaffixes/suffix/a006b_ch_att_physpi_10.dbr"}))

  (time
   (construct-item "mythical stormheart" (dbu/db) (dbu/db-and-index) nil))

  (time
   (construct-item "legion warhammer of valor" (dbu/db) (dbu/db-and-index) nil))

  (time
   (construct-item "stonehide Dreeg-Sect Legguards of the boar" (dbu/db) (dbu/db-and-index) 50))

  (time
   (construct-item "stanching chain belt of caged souls" (dbu/db) (dbu/db-and-index) 12))

  (time
   (construct-item "Lokarr's Gaze" (dbu/db) (dbu/db-and-index) nil))

  (u/timed-readable
   (def item-name-idx
     (build-item-name-idx (dbu/db))))

  (u/timed-readable (count item-name-idx))

  (u/timed-readable
   (def
     affixes-all (->> (dbu/db)
                      (reshape-records-as-affixes)
                      (group-affixes-by-loot-name))))

  (u/timed-readable (count affixes-all))

  (analyze-item-name item-name-idx affixes-all "stanching chain belt of caged souls")

  (analyze-item-name item-name-idx affixes-all "wraithbound infantry waistguard of vitality")

  (time
   (construct-item "badge of mastery" (dbu/db) (dbu/db-and-index) nil))


  (->> @globals/db
       (filter #(u/ci-match (:recordname %) "/items/crafting/blueprints/") )
       first
       )

  (->> @globals/db
       all-loottable-records
       (map dbu/record-class)
       distinct
       )

  )
