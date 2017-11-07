(ns gd-edit.printer
  "Deals with printing output of structured data"
  (:require [gd-edit
             [db-utils :as dbu]
             [utils :as u]]
            [jansi-clj.core :refer :all]
            [gd-edit.db-utils :as dbu]))


(declare show-item)

(defn print-primitive
  "Prints numbers, strings, booleans, and byte arrays"
  ([obj]
   (print-primitive obj 0))

  ([obj indent-level]
   (cond
     (or (number? obj) (string? obj) (boolean? obj))
     (do
       (u/print-indent indent-level)
       (println (yellow obj)))

     (u/byte-array? obj)
     (do
       (u/print-indent indent-level)
       (println (format "byte array[%d]" (count obj))
                (map #(format (yellow "%02X") %1) obj))))))

(defn print-map
  [character-map & {:keys [skip-item-count]
                    :or {skip-item-count false}}]

  (let [character (->> character-map
                       (filter dbu/without-meta-fields)
                       (sort-by #(str (first %))))

        max-key-length (reduce
                        (fn [max-length key-str]
                          (if (> (count key-str) max-length)
                            (count key-str)
                            max-length))
                        0

                        ;; Map the keys to a more readable string format
                        (->> character
                             (keys)
                             (map u/keyword->str))
                        )]

    (doseq [[key value] character]
      (println

       ;; Print the key name
       (format (format "%%%ds :" (+ max-key-length 2))
               (u/keyword->str key))

       ;; Print the value
       (cond
         (coll? value)
         (format "collection of %d items" (count value))

         (and (string? value) (empty? value))
         "\"\""

         :else
         (yellow value))))

    (when-not skip-item-count
      (newline)
      (println (format (format "%%%dd" (+ max-key-length 2)) (count character)) "fields"))))

(defn print-sequence
  [obj]

  (u/doseq-indexed i [item obj]
                   ;; Print the index of the object
                   (print (format
                           (format "%%%dd: " (-> (count obj)
                                                 (Math/log10)
                                                 (Math/ceil)
                                                 (max 1)
                                                 (int)))
                           i))

                   ;; Print some representation of the object
                   ;; We don't call print-object here
                   ;; because we do not want to recursively print the data tree.
                   (let [item-type (type item)]
                     (cond
                       (dbu/is-primitive? item)
                       (print-primitive item)

                       (sequential? item)
                       (println (format "collection of %d items" (count item)))

                       (associative? item)
                       (do

                         ;; If we've encountered something that requires annotation,
                         ;; print the annotation now.
                         (let [annotation
                               (cond
                                 (dbu/is-item? item)
                                 (dbu/item-name item @gd-edit.globals/db)

                                 (dbu/is-skill? item)
                                 (dbu/skill-name item)

                                 (dbu/is-faction? item)
                                 (dbu/faction-name i)

                                 :else
                                 nil)]
                               (if-not (nil? annotation)
                                 (do
                                   ;; Print annotation on the same line as the index
                                   (println (yellow annotation))
                                   (newline))

                                 ;; No annotation needs to be printed,
                                 ;; close the line that prints the index
                                 (newline))
                               )

                         (print-map item :skip-item-count true)
                         (if-not (= i (dec (count obj)))
                           (newline))
                         )

                       :else
                       (println item)))))

(defn print-object
  "Given some kind of thing in the character sheet, try to print it."
  [obj]

  (let [t (type obj)]
    (cond
      (dbu/is-primitive? obj)
      (print-primitive obj 1)

      (sequential? obj)
      (print-sequence obj)

      ;; If we're looking at an item, print all of its details and related records
      (dbu/is-item? obj)
      (show-item obj)

      (associative? obj)
      (print-map obj)

      :else
      (throw (Throwable. "Unhandled case"))
      )))

(defn print-details
  [data]

  (assert (associative? data))
  (assert (= (count data) 1))

  (let [[key value] (first data)]
    (println (format "%s:" (u/keyword->str key)))

    (cond
      (or (number? value) (string? value))
      (do
        (print "        ")
        (println value))

      (sequential? value)
      (do
        (u/doseq-indexed i [item value]
                         (println i ":")
                         (cond
                           (sequential? item)
                           (println
                            (format "collection of %d items" (count item)))

                           (associative? item)
                           (do
                             (print-map item :skip-item-count true)
                             (if-not (= item (last value))
                               (newline)))

                           :else
                           (throw (Throwable. "Don't know how to print details for a sequence of this type yet!")))
                         )))))

(defn print-result-records
  [results]
  {:pre [(sequential? results)]}

  (doseq [kv-map results]
    (println (:recordname kv-map))
    (doseq [[key value] (->> kv-map
                             seq
                             (filter #(not (keyword? (first %1))))
                             (sort-by first))]

      (println (format "\t%s: %s" key (yellow value))))

    (newline)))

(defn print-map-difference
  [[only-in-a only-in-b _]]

  (if (empty? only-in-b)
    (println "Nothing has been changed")

    (let [max-key-length (->> (keys only-in-b)
                              (map (comp count u/keyword->str))
                              (apply max))]

      (doseq [[key value] (sort only-in-b)]
        (println

         ;; Print the key name
         (format (format "%%%ds :" (+ max-key-length 2))
                 (u/keyword->str key))

         ;; Print the value
         (cond
           (coll? value)
           (format "collection with %d items changed" (->> value
                                                         (filter some?)
                                                         (count)))

           :else
           (do
             ;; (println (format "%s => %s" (yellow (type value)) (yellow (type (only-in-b key)))))
             (format "%s => %s" (yellow value) (yellow (only-in-b key)))))))

      (newline)
      (println (format (format "%%%dd" (+ max-key-length 2)) (count only-in-b)) "fields changed"))))

(defn show-item
  "Print all related records for an item"
  [item]

  (cond

    (not (dbu/is-item? item))
    (println "Sorry, this doesn't look like an item")

    (empty? (:basename item))
    (do
      (println "This isn't a valid item (no basename)")
      (print-map item))

    :else
    (let [related-records (dbu/related-db-records item @gd-edit.globals/db)
          name (dbu/item-name item @gd-edit.globals/db)]
      (when (not (nil? name))
        (println (yellow name))
        (newline))
      (print-map item :skip-item-count true)
      (newline)
      (print-result-records related-records))))
