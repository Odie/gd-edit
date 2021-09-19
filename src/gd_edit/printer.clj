(ns gd-edit.printer
  "Deals with printing output of structured data"
  (:require [gd-edit
             [db-utils :as dbu]
             [utils :as u]
             [jline :as jline]
             [item-summary :as item-summary]]
            [clojure.string :as str]
            [jansi-clj.core :refer [red green yellow]]
            [gd-edit.printer :as printer]
            [gd-edit.globals :as globals]))


(defn prompt-yes-no
  "Prompts the user to answer Y or N, returns true or false depending on user input."
  ([prompt]
   (prompt-yes-no prompt nil))

  ([prompt default]
   (let [prompt (cond->> prompt
                  (vector? prompt) (str/join "\n"))
         print-yes-no (fn [default]
                        (u/print- (cond
                                    (true? default) "(Y/n) "
                                    (false? default) "(y/N) "
                                    (nil? default) "(y/n) ")))]

     ;; Show the prompt
     (u/print- prompt)
     (u/print- " ")
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
                 (u/print-line)
                 (u/print-line "Please enter 'Y' or 'N'.")
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
       (u/print-line (yellow obj)))

     (u/byte-array? obj)
     (do
       (u/print-indent indent-level)
       (u/print-line (format "byte array[%d]" (count obj))
                (map #(format (yellow "%02X") %1) obj))))))

(defn print-kvs
  [kvs & {:keys [skip-item-count]
            :or {skip-item-count false}}]

  (let [max-key-length (reduce
                        (fn [max-length key-str]
                          (if (> (count key-str) max-length)
                            (count key-str)
                            max-length))
                        0

                        ;; Map the keys to a more readable string format
                        (->> kvs
                             (map first)
                             (map u/keyword->str)))]

    (doseq [[key value] kvs]
      (u/print-line

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
      (u/newline-)
      (u/print-line (format (format "%%%dd" (+ max-key-length 2)) (count kvs)) "fields"))))

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
      (u/print-line

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
      (u/newline-)
      (u/print-line (format (format "%%%dd" (+ max-key-length 2)) (count character)) "fields"))))


(defn print-uid-summary
  [obj]
  (if-let [uid-info (@globals/shrines-and-gates-index (seq obj))]
    (let [record (dbu/record-by-name (:recordname uid-info))]
      (u/print-line (yellow (dbu/uid-record-display-name record))
               ;; (format (green "[%s]")  (uid-record-type-display-name record))
               ))
    (print-primitive obj)))

(defn- print-uid
  [obj]
  (when-let [uid-info (@globals/shrines-and-gates-index (seq obj))]
    (let [record (dbu/record-by-name (:recordname uid-info))]
      (u/print-line (yellow (dbu/uid-record-display-name record)))
      (u/print-line (yellow (dbu/uid-record-type-display-name record)))
      (u/print-line)))
  (print-primitive obj 1))

(defn print-sequence
  [obj path]

  (if (empty? obj)
    (do
      (u/print-indent 1)
      (u/print-line (yellow "Empty")))
    (doseq [[i item] (u/with-idx obj)]
      ;; Print the index of the object
      (u/print- (format
                 (format "%%%dd: " (-> (count obj)
                                       (Math/log10)
                                       (Math/ceil)
                                       (max 1)
                                       (int)))
                 i))

      ;; Print some representation of the object
      (cond
        (dbu/uid? item)
        (print-uid-summary item)

        (dbu/is-primitive? item)
        (print-primitive item)

        (sequential? item)
        (u/print-line (format "collection of %d items" (count item)))

        (associative? item)
        (do
          ;; If a display name can be fetched...
          (when-let [display-name (dbu/get-name item (conj path i))]
            ;; Print annotation on the same line as the index
            (u/print-line (yellow display-name)))

          ;; Close the index + display-name line
          (u/newline-)

          (print-map item :skip-item-count true)
          (if-not (= i (dec (count obj)))
            (u/newline-)))

        :else
        (u/print-line item)))))


(defn- print-object-name
  [s]

  (when-not (empty? s)
    (u/print-line (yellow s))
    (u/print-line)))

(defn print-object
  "Given some kind of thing in the character sheet, try to print it."
  [obj path]

  (cond
    ;; For items, we want to fetch and print all related db records.
    (= (dbu/get-type obj) :item)
    (show-item obj)

    (dbu/uid? obj)
    (print-uid obj)

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
      (u/print-line s))

    ;; Otherwise, we'll assume we're looking at a db record...
    ;; Print the record
    (doseq [kv-map results]
      (u/print-line (:recordname kv-map))
      (doseq [[key value] (->> kv-map
                               seq
                               (filter #(not (keyword? (first %1))))
                               (sort-by first))]

        (u/print-line (format "\t%s: %s" key (yellow value))))

      (u/newline-))))


(defn print-map-difference
  [[only-in-a only-in-b _]]

  (if (and (empty? only-in-a) (empty? only-in-b))
    (u/print-line "Nothing has been changed")

    (let [changed-keys (->> (concat (keys only-in-a)
                                    (keys only-in-b))
                            (into #{}))
          max-key-length (->> changed-keys
                              (map (comp count u/keyword->str))
                              (apply max 0))]

      (doseq [[key value] (sort only-in-a)]
        (u/print-line

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

      (u/newline-)
      (u/print-line (format (format "%%%dd" (+ max-key-length 2)) (count changed-keys)) "fields changed"))))


(defn show-item
  "Print all related records for an item"
  [item]

  (cond

    (not (dbu/is-item? item))
    (u/print-line "Sorry, this doesn't look like an item")

    (empty? (:basename item))
    (do
      (u/print-line "This isn't a valid item (no basename)")
      (print-map item))

    :else
    (let [related-records (dbu/related-db-records item (dbu/db-and-index))
          name (dbu/item-name item (dbu/db-and-index))]
      (when (not (nil? name))
        (u/print-line (yellow name))
        (u/newline-))
      (print-map item :skip-item-count true)
      (u/newline-)
      (print-result-records related-records)

      (try
        (let [summary (item-summary/item-summary item)]
          (u/print-line (first summary))
          (doseq [line (rest summary)]
            (u/print-indent 1)
            (u/print-line line)))
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
