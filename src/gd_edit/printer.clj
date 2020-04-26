(ns gd-edit.printer
  "Deals with printing output of structured data"
  (:require [gd-edit
             [db-utils :as dbu]
             [utils :as u]
             [jline :as jline]
             [item-summary :as item-summary]]
            [clojure.string :as str]
            [jansi-clj.core :refer [red green yellow]]
            [gd-edit.printer :as printer]))

(defn prompt-yes-no
  "Prompts the user to answer Y or N, returns true or false depending on user input."
  ([prompt]
   (prompt-yes-no prompt nil))

  ([prompt default]
   (let [prompt (cond->> prompt
                  (vector? prompt) (str/join "\n"))
         print-yes-no (fn [default]
                        (print (cond
                                 (true? default) "(Y/n) "
                                 (false? default) "(y/N) "
                                 (nil? default) "(y/n) ")))]

     ;; Show the prompt
     (print prompt)
     (print " ")
     (print-yes-no default)
     (flush)

     ;; Read the user's answer
     (let [line (str/lower-case (jline/readline))]
       (cond
         (str/starts-with? line "y") true
         (str/starts-with? line "n") false

         ;; If the user didn't supply an answer...
         ;; and the caller has set the default value...
         ;; Just return the default value
         (and (empty? line)
              (some? default)) default

         ;; Otherwise, ask the user to choose from a valid answer.
         :else (do
                 (println)
                 (println "Please enter 'Y' or 'N'.")
                 (recur prompt default)))))))

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

         (u/byte-array? value)
         (format "byte array[%d]" (count value))

         (and (string? value) (empty? value))
         "\"\""

         :else
         (yellow value))))

    (when-not skip-item-count
      (newline)
      (println (format (format "%%%dd" (+ max-key-length 2)) (count character)) "fields"))))


(defn print-sequence
  [obj path]

  (if (empty? obj)
    (do
      (u/print-indent 1)
      (println (yellow "Empty")))
    (doseq [[i item] (u/with-idx obj)]
      ;; Print the index of the object
      (print (format
              (format "%%%dd: " (-> (count obj)
                                    (Math/log10)
                                    (Math/ceil)
                                    (max 1)
                                    (int)))
              i))

      ;; Print some representation of the object
      (cond
        (dbu/is-primitive? item)
        (print-primitive item)

        (sequential? item)
        (println (format "collection of %d items" (count item)))

        (associative? item)
        (do
          ;; If a display name can be fetched...
          (when-let [display-name (dbu/get-name item (conj path i))]
            ;; Print annotation on the same line as the index
            (println (yellow display-name)))

          ;; Close the index + display-name line
          (newline)

          (print-map item :skip-item-count true)
          (if-not (= i (dec (count obj)))
            (newline)))

        :else
        (println item)))))


(defn- print-object-name
  [s]

  (when-not (empty? s)
    (println (yellow s))
    (println)))

(defn print-object
  "Given some kind of thing in the character sheet, try to print it."
  [obj path]

  (cond
    ;; For items, we want to fetch and print all related db records.
    (= (dbu/get-type obj) :item)
    (show-item obj)

    ;; For other types of interesting data, print out the
    ;; name followed by the data itself.
    (dbu/get-type obj)
    (do
      (print-object-name (dbu/get-name obj path))
      (print-map obj))

    (dbu/is-primitive? obj)
    (print-primitive obj 1)

    (sequential? obj)
    (print-sequence obj path)

    :else
    (print-map obj)))


(defn print-result-records
  [results]
  {:pre [(sequential? results)]}

  ;; The results might just be a list of strings
  (if (string? (first results))
    ;; In that case, just show the strings
    (doseq [s results]
      (println s))

    ;; Otherwise, we'll assume we're looking at a db record...
    ;; Print the record
    (doseq [kv-map results]
      (println (:recordname kv-map))
      (doseq [[key value] (->> kv-map
                               seq
                               (filter #(not (keyword? (first %1))))
                               (sort-by first))]

        (println (format "\t%s: %s" key (yellow value))))

      (newline))))


(defn print-map-difference
  [[only-in-a only-in-b _]]

  (if (and (empty? only-in-a) (empty? only-in-b))
    (println "Nothing has been changed")

    (let [changed-keys (->> (concat (keys only-in-a)
                                    (keys only-in-b))
                            (into #{}))
          max-key-length (->> changed-keys
                              (map (comp count u/keyword->str))
                              (apply max 0))]

      (doseq [[key value] (sort only-in-a)]
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
           (format "%s => %s" (yellow value) (yellow (only-in-b key))))))

      (newline)
      (println (format (format "%%%dd" (+ max-key-length 2)) (count changed-keys)) "fields changed"))))


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
    (let [related-records (dbu/related-db-records item (dbu/db-and-index))
          name (dbu/item-name item (dbu/db-and-index))]
      (when (not (nil? name))
        (println (yellow name))
        (newline))
      (print-map item :skip-item-count true)
      (newline)
      (print-result-records related-records)

      (try
        (let [summary (item-summary/item-summary item)]
          (println (first summary))
          (doseq [line (rest summary)]
            (u/print-indent 1)
            (println line)))
        (catch Exception e)))))


(defn displayable-path
  "Given a path into a data structure (such as paths returned by collect-walk*), and turn it
  into a string suitable for display."
  [path]
  (->> path
       (map #(cond
               (keyword? %) (name %)
               :else %))
       (str/join "/")))
