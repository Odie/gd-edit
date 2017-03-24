(ns gd-edit.db-utils
  (:require [clojure.string :as str]
            [gd-edit.globals :as globals]))

(defn record-by-name
  [recordname]

  (@globals/db-index recordname))

(defn related-db-records
  [record db]

  (let [;; Collect all values in the record that look like a db record
        related-recordnames (->> record
                                 (reduce (fn [coll [key value]]
                                           (if (and (string? value) (.startsWith value "records/"))
                                             (conj coll value)
                                             coll
                                             ))
                                         #{}))

        ;; Retrieve all related records by name
        related-records (filter #(contains? related-recordnames (:recordname %1)) db)]

    related-records))

(defn item-base-record-get-base-name
  [base-record]

  (or (get base-record "itemNameTag") (-> (get base-record "description")
                                          (str/replace "^k" ""))))
(defn record-has-display-name?
  [record]

  (or (get record "itemNameTag") (get record "description")))

(defn item-base-record-get-quality-name
  [base-record]

  (or (base-record "itemQualityTag") (base-record "itemStyleTag")))

(defn item-base-record-get-name
  [item-base-record]

  (let [base-name (item-base-record-get-base-name item-base-record)
        quality-name (item-base-record-get-quality-name item-base-record)]

    (->> [quality-name base-name]
         (filter #(not (nil? %1)))
         (str/join " "))))

(defn- is-valid-item?
  [item]

  (if (or  (nil? (:basename item)) (empty? (:basename item)))
    false
    true))

(defn item-name
  [item db]

  (if-not (is-valid-item? item)
    nil

    (let [related-records (related-db-records item db)
          base-record (-> (filter #(= (:basename item) (:recordname %1)) related-records)
                          (first))

          base-name (item-base-record-get-base-name base-record)

          is-set-item (some #(contains? %1 "itemSetName") related-records)]

      ;; If we can't find a base name for the item, this is not a valid item
      ;; We can't generate a name for an invalid item
      (if (not (nil? base-name))

        ;; If we've found an item with a unique name, just return the name without any
        ;; prefix or suffix
        (if is-set-item
          base-name

          ;; Otherwise, we should fetch the prefix and suffix name to construct the complete name
          ;; of the item
          (let [prefix-name (-> (filter #(str/includes? (:recordname %1) "/prefix/") related-records)
                                (first)
                                (get "lootRandomizerName"))
                suffix-name (-> (filter #(str/includes? (:recordname %1) "/suffix/") related-records)
                                (first)
                                (get "lootRandomizerName"))
                quality-name (item-base-record-get-quality-name base-record)]

            (->> [prefix-name quality-name base-name suffix-name]
                 (filter #(not (nil? %1)))
                 (str/join " "))))))))
