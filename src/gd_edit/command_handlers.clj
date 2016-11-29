(ns gd-edit.command-handlers
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [gd-edit
             [arc-reader :as arc-reader]
             [arz-reader :as arz-reader]
             [db-query :as query]
             [game-dirs :as dirs]
             [gdc-reader :as gdc]
             [globals :as globals]
             [utils :as u]]
            [jansi-clj.core :refer :all]
            ))

(declare is-item? show-item set-item-handler)

(defn paginate-next
  [page {:keys [pagination-size] :as query-state}]

  ;; How many results do we have?
  (let [size (count (:result query-state))

        ;; How many items have we shown?
        shown-so-far (* (inc (or page 0)) pagination-size)]

    ;; If we haven't shown everything yet...
    (if (< shown-so-far size)
      ;; Show the next page
      (inc page)

      ;; If we've already shown everything...
      ;; Go back to the beginning
      0
      )))

(defn paginate-next!
  [query-state-atom]
  (swap! query-state-atom update :page (fn [oldval] (paginate-next oldval @query-state-atom))))


(defn get-paginated-result
  [result {:keys [page pagination-size] :as query-state}]

  (->> result
       (drop (* page pagination-size))
       (take pagination-size)))


(defn set-new-query-result!
  [query-state-atom new-result query-string]

  (reset! globals/query-state (assoc @globals/query-state
                                     :query-string query-string
                                     :result new-result
                                     :page 0)))


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


(defn print-paginated-result
  [query-state]

  (let [{:keys [result page pagination-size]} query-state
        paginated-result (get-paginated-result result query-state)
        start-entry (* page pagination-size)
        end-entry (+ start-entry (min (count paginated-result) pagination-size))]

    (print-result-records paginated-result)
    (println)
    (println (format "%d-%d / %d %s" start-entry end-entry (count result) (yellow "matched records")))))


(defn query-show-handler
  [[input tokens]]

  (paginate-next! globals/query-state)
  (print-paginated-result @globals/query-state))


(defn run-query
  [input]

  (let [predicates (try (query/query-string->query-predicates input)
                        (catch Throwable e (println (str "Query syntax error: " (.getMessage e)))))]

    (when (not (nil? predicates))

      ;; Run a query against the db using generated predicates
      (set-new-query-result! globals/query-state
                             (query/query @globals/db predicates)
                             input)

      (print-paginated-result @globals/query-state))))

(defn query-comand-handler
  [[input tokens]]

  (cond
    (empty? input)
    (println
     "usage: q <target> <op> <value>
       <target> can be \"recordname\", \"value\", or \"key\"")

    (not (realized? globals/db))
    (println "db not yet loaded")

    :else
    (run-query input)))

(defn string-first-n-path-components
  "Given a path, return a path with the first n path components"
  [n path]

  (clojure.string/join "/" (take n (clojure.string/split path #"/"))))

(defn db-partial-record-path-matches
  [db path]

  (let [paths (->> db
                   (map :recordname))]

    ;; Generate a set of paths such that...
    (reduce (fn [accum item]

              ;; If the items first n components are a partial match to the user specified path
              (if (every? (fn [item-pair]
                            (u/ci-match (second item-pair) (first item-pair)))

                          (partition 2 (interleave (clojure.string/split path #"/")
                                                   (clojure.string/split item #"/"))))

                ;; Take the string of the first n+1 components and put it in the set
                (conj accum item)
                accum
                ))

            []
            paths)))

(defn db-show-handler
  [[input tokens]]

  (let [path (if (nil? (first tokens))
               "r/"
               (first tokens))]

    (let [sorted-matches

          ;; Grab a list of items from the db that partially matches the specified path
          (->> (db-partial-record-path-matches @gd-edit.globals/db path)

               ;; Consolidate the search so we only have n+1 path components
               (reduce (fn [accum item]
                         (conj accum
                               (string-first-n-path-components (inc (count (clojure.string/split path #"/"))) item)))
                       #{})

               ;; Sort the matches
               (sort))

          ;; It's possible for the user to target a single record using "db show".
          ;; It that happens, we should just display the contents of that record instead of a
          ;; list of matched paths
          ;; Try to fetch the full recordname of the match now
          sole-match-recordname
          (if (= 1 (count sorted-matches))
            (first (db-partial-record-path-matches @gd-edit.globals/db path))
            nil)]

      ;; If we have a single record match
      (if sole-match-recordname

        ;; Locate the record by name
        (->> (filter (fn [item]
                       (= (:recordname item) sole-match-recordname))
                     @gd-edit.globals/db)

             ;; Print the record
             (print-result-records))


        ;; If we have multiple record matches...
        ;; Just print the names
        (do
          (doseq [item sorted-matches]
            (println item))
          (println)
          (println (count sorted-matches) " matches"))))))

(defn- last-path-component
  [path]

  (last (clojure.string/split path #"[/\\]")))

(declare load-character-file
         write-character-file
         character-selection-screen!
         character-manipulation-screen!)

(defn- character-manipulation-screen
  []

  {:display-fn
   (fn []
     (println "Character: " (:character-name @globals/character)))

   :choice-map [["r" "reload" (fn [] (load-character-file @globals/last-character-load-path))]
                ["w" "write" (fn[] (write-character-file @globals/character))]]})

(defn- character-selection-screen
  []

  ;; grab a list save directories where a "player.gdc" file exists
  (let [save-dirs (->> (dirs/get-save-dir)
                       (io/file)
                       (.listFiles)
                       (filter #(.isDirectory %1))
                       (filter #(.exists (io/file %1 "player.gdc"))))]

    {:display-fn
     (fn []
       (println "Please choose a character to load:"))

     ;; generate the menu choices
     ;; reduce over save-dirs with each item being [index save-dir-item]
     :choice-map (reduce
                  (fn [accum [idx dir]]
                    (let [display-idx (inc idx)]

                      (conj accum
                            [(str display-idx)                    ; command string
                             (last-path-component (.getPath dir)) ; menu display string
                             (fn []                               ; function to run when selected
                               (let [savepath (.getPath (io/file dir "player.gdc"))]
                                 (load-character-file savepath)
                                 (character-manipulation-screen!))
                               )])))
                  []
                  (map-indexed vector save-dirs))
     }))

(defn character-selection-screen! [] (reset! gd-edit.globals/menu (character-selection-screen)))
(defn character-manipulation-screen! [] (reset! gd-edit.globals/menu (character-manipulation-screen)))

(defn choose-character-handler
  [[input tokens]]

  character-selection-screen!)

(defn- character-loaded?
  []
  (and (not (nil? @gd-edit.globals/character))
       (not (empty? @gd-edit.globals/character))))

(defn- keyword->str
  [k]
  (if (keyword? k)
    (subs (str k) 1)
    k))

(defn- without-meta-fields
  [kv-pair]

  (not (.startsWith (str (first kv-pair)) ":meta-")))

(defn- without-recordname
  [[key value]]
  (not= key :recordname))

(defn print-map
  [character-map & {:keys [skip-item-count]
                    :or {skip-item-count false}}]

  (let [character (->> character-map
                       (filter without-meta-fields)
                       (sort-by first))

        max-key-length (reduce
                        (fn [max-length key-str]
                          (if (> (count key-str) max-length)
                            (count key-str)
                            max-length))
                        0

                        ;; Map the keys to a more readable string format
                        (->> character
                             (keys)
                             (map keyword->str))
                        )]

    (doseq [[key value] character]
      (println

       ;; Print the key name
       (format (format "%%%ds :" (+ max-key-length 2))
               (keyword->str key))

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

(defn str-without-dash
  [str]
  (string/replace str "-" ""))

(defn- partially-match-key
  "Given a map and a partial key, return a list of pairs that partially matches the key"
  [m search-target]

  (let [haystack (filter without-meta-fields m)

        ;; Locate all partial matches
        partial-matches (filter (fn [[key value]]
                                  (let [k-str (keyword->str key)]
                                    (or (u/ci-match k-str search-target)
                                        (u/ci-match (str-without-dash k-str) search-target))))
                                haystack)

        ;; Are any of the partial matches actual exact matches?
        ;; If we don't look for
        exact-match (filter (fn [[key value]]
                              (let [k-str (keyword->str key)]
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
    (get coll (coerce-to-int key))

    (associative? coll)
    (get coll key)

    :else
    (throw (Throwable. "Can't deal with coll with type: " (type coll)))))

(defn walk-structure
  [m ks-all]

  (letfn [;; We want to return a slightly more complex set of data to the caller.
          ;; This helper function helps us construct the return map so we don't have
          ;; to sprinkle this everywhere.
          (return-result [result end-ks actual-path & others]
            (merge {;; Report the result the caller said to report
                    :status result

                    ;; Calculate the longest path traversed
                    :longest-path (take (- (count ks-all) (count end-ks)) ks-all)
                    :actual-path actual-path
                    }

                   (first others))
            )]
    ;; For each item in the specified ks
    ;; Try to iterate a level deeper using the next key
    (loop [cursor m
           ks ks-all
           actual-path []
           ]

      ;; Which key are we trying to navigate to?
      (let [k (first ks)]
        (cond
          ;; Did we try navigating into a location that doesn't exist?
          (nil? cursor)
          (return-result :not-found ks actual-path)

          ;; Did we exhaust all the keys?
          ;; If so, we're done navigating into the hierarchy.
          ;; The cursor should be pointing at the item the user wants
          (nil? k)
          (return-result :found ks actual-path {:found-item cursor})

          ;; If we have a sequential collection, just try to navigate into
          ;; the collection with the key
          (sequential? cursor)
          (recur (nav-into cursor k) (rest ks) (conj actual-path (coerce-to-int k)))

          ;; If we're looking at an associative collection, then we want to
          ;; perform partial matching on the current key
          (associative? cursor)
          (let [matches (partially-match-key cursor k)]
            (cond
              ;; If we can't get a match at all, we cannot navigate to the key
              (= (count matches) 0)
              (return-result :not-found (rest ks) actual-path)

              ;; If we have more than one match, tell the caller we cannot
              ;; resolve this ambiguity.
              (> (count matches) 1)
              (return-result :too-many-matches (rest ks) actual-path {:ambiguous-matches matches})

              :else
              (let [[matched-key matched-value] (first matches)]
                (recur matched-value (rest ks) (conj actual-path matched-key)))))

          :else
          (return-result :cannot-traverse ks actual-path))))))


(defn print-details
  [data]

  (assert (associative? data))
  (assert (= (count data) 1))

  (let [[key value] (first data)]
    (println (format "%s:" (keyword->str key)))

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

(defn print-indent
  [indent-level]

  (dotimes [i indent-level]
    (print "    ")))

(defn- is-primitive?
  [val]

  (or (number? val) (string? val) (u/byte-array? val)))

(defn print-primitive
  ([obj]
   (print-primitive obj 0))

  ([obj indent-level]
   (cond
     (or (number? obj) (string? obj) )
     (do
       (print-indent indent-level)
       (println (yellow obj)))

     (u/byte-array? obj)
     (do
       (print-indent indent-level)
       (println (format "byte array[%d]" (count obj))
                (map #(format (yellow "%02X") %1) obj)))
     )))

(defn print-object
  [obj]

  (let [t (type obj)]
    (cond
      (is-primitive? obj)
      (print-primitive obj 1)

      (sequential? obj)
      (u/doseq-indexed i [item obj]
                       (print (format
                               (format "%%%dd: " (-> (count obj)
                                                     (Math/log10)
                                                     (Math/ceil)
                                                     (max 1)
                                                     (int)))
                               i))
                       (let [item-type (type item)]
                         (cond
                           (is-primitive? item)
                           (print-primitive item)

                           (sequential? item)
                           (println (format "collection of %d items" (count item)))

                           (associative? item)
                           (do
                             (newline)
                             (print-map item :skip-item-count true)
                             (if-not (= i (dec (count obj)))
                               (newline))
                             )

                           :else
                           (println item))))

      (is-item? obj)
      (show-item obj)

      (associative? obj)
      (print-map obj)

      :else
      (throw (Throwable. "Unhandled case"))
      )))

(defn print-ambiguous-walk-result
  [result]

  (println (format "Cannot traverse path because \"%s\""
                   (clojure.string/join "/" (:longest-path result)))
           "matches more than one item:")

  (doseq [ambiguous-item (->> (:ambiguous-matches result)
                              (keys)
                              (map keyword->str))]
    (print-indent 1)
    (println ambiguous-item)))

(defn- classname
  [obj]

  (cond
    (u/byte-array? obj)
    "byte array"

    (integer?)
    "integer"

    (float?)
    "float"

    (string?)
    "string"
    )
  )

(defn print-cannot-traverse-walk-result
  [result]

  (let [actual-path-item (get-in @globals/character (:actual-path result))]
    (println (format "Cannot traverse into %s at the path \"%s\""
                     (classname actual-path-item)
                     (clojure.string/join "/" (:longest-path result))))))

(defn show-handler
  [[input tokens]]

  (cond
    ;; If the character hasn't been loaded...
    ;; Move to the character selection screen first
    (not (character-loaded?))
    (character-selection-screen!)

    ;; The character is loaded
    ;; If there aren't any filter conditions, just display all the character fields
    (= (count tokens) 0)
    (print-map @globals/character)

    :else
    (do
      ;; Split a path into components.
      ;; We're going to use these as keys to navigate into the character sheet
      (let [path-keys (string/split (first tokens) #"/")
            base-obj (if-not (> (count path-keys) 1)
                        @globals/character

                        ;; If the user supplied a long path, we should try to walk the
                        ;; data structure first and arrive at a base object
                        (let [result (walk-structure @globals/character (butlast path-keys))
                              {:keys [status found-item]} result]

                          (cond
                            (= status :not-found)
                            (println "No matches found")

                            (= status :too-many-matches)
                            (print-ambiguous-walk-result result)

                            (= status :cannot-traverse)
                            (print-cannot-traverse-walk-result result)

                            :else
                            (:found-item result)))
              )]

        ;; Now that we've found a base object to navigate into...
        (when-not (nil? base-obj)
          ;; Walk into it one more level...
          (let [result (walk-structure base-obj [(last path-keys)])
                {:keys [status]} result

                matched-obj (cond
                              (= status :not-found)
                              (println "No matches found")

                              (= status :too-many-matches)
                              (into {} (:ambiguous-matches result))

                              (= status :cannot-traverse)
                              (let [result (walk-structure @globals/character path-keys)]
                                (print-cannot-traverse-walk-result result))

                              ;; If we found a result and it looks like a primitive...
                              ;; We want to print the path to that primitive then the value
                              (and (= status :found) (is-primitive? (:found-item result)))
                              (let [result (walk-structure @globals/character path-keys)]
                                (println (->> (:actual-path result)
                                              (map keyword->str)
                                              (string/join "/")))
                                (:found-item result))

                              (= status :found)
                              (:found-item result)

                              :else
                              (throw (Throwable. "Unhandled case")))
                ]

            ;; We've successfully navigated to an object
            ;; This might be a piece of data in the character sheet or a list of partially matched result
            ;; for some structure represented as a map
            (if-not (nil? matched-obj)
              (print-object matched-obj))))))))


(defn coerce-to-type
  "Given an value as a string, return the value after coercing it to the correct type."
  [val-str type]

  (cond
    ;; If the caller asked to convert the value to a string,
    ;; we don't have to do anything because the value should already be a string
    (= java.lang.String type)
    val-str

    (= java.lang.Float type)
    (Float/parseFloat val-str)

    (= java.lang.Integer type)
    (Integer/parseInt val-str)))


(defn set-handler
  [[input tokens]]

  (cond
    ;; If the character hasn't been loaded...
    ;; Move to the character selection screen first
    (not (character-loaded?))
    (character-selection-screen!)

    (< (count tokens) 2)
    (println "Usage: show <path> <new-value>")

    :else
    (do
      ;; Split a path into components.
      ;; We're going to use these as keys to navigate into the character sheet
      (let [path-keys (string/split (first tokens) #"/")
            walk-result (walk-structure @globals/character path-keys)
            {:keys [status]} walk-result]

        (cond
          (= status :not-found)
          (println "No matches found")

          (= status :too-many-matches)
          (print-ambiguous-walk-result walk-result)

          (= status :found)
          (do
            ;; We found the value the user wanted to update
            (let [value (:found-item walk-result)
                  ;; What is the type of the value? We'll have to coerce the supplied new value
                  ;; to the same type first.
                  newval (try (coerce-to-type (second tokens) (type value))
                              (catch Exception e :failed))]

              (cond
                ;; Is the targetted value an item?
                ;; Let the set-item handler deal with it
                (is-item? value)
                (set-item-handler [input tokens])

                ;; The user cannot create a collection directly from the commandline.
                ;; So replacing a collection directly makes no sense and cannot be done.
                (coll? value)
                (println "Sorry, can't set the value of" (first tokens))

                ;; If coercion failed, tell the user what type is being expected
                (= newval :failed)
                (println "Please provide a value that is of type" (-> (type value)
                                                                      (str)
                                                                      (string/split #"\.")
                                                                      (last)
                                                                      (string/lower-case)))

                :else
                ;; Set the new value into the character sheet
                (reset! globals/character
                        (update-in @globals/character (:actual-path walk-result) (fn [oldval] newval)))
                )
              nil
              ))

          :else
          (throw (Throwable. "Unhandled case")))
        ))))

(defn- get-savepath
  [character-name]
  (io/file (dirs/get-save-dir) (format "_%s" character-name) "player.gdc"))

(defn- get-new-backup-path
  [character-name]

  ;; Grab all sibling files of the character save path
  (let [sibling-files (-> (get-savepath character-name)
                          (.getParentFile)
                          (.listFiles))

        save-backups (->> sibling-files
                          (filter #(.isFile %1))
                          (filter #(not (nil? (re-matches #"player\.gdc\..*$" (.getName %1))))))]

    (format "%s.bak%d" (get-savepath character-name) (inc (count save-backups)))))

(defn- load-character-file
  [savepath]

  (reset! globals/character
          (gdc/load-character-file savepath))
  (reset! globals/last-character-load-path
          savepath))

(defn- write-character-file
  [character]

  ;; Figure out where we're going to write to
  (let [character-name (:character-name character)
        savepath (get-savepath character-name)

        ;; In case the user renamed the character, the save path may not exist at all
        ;; Make sure all parent directories are created
        _ (io/make-parents savepath)

        backup-path (io/file (get-new-backup-path character-name))]

    ;; Backup the current save file
    (when (.exists savepath)
      (.renameTo savepath backup-path))

    ;; Write the new character file
    (gd-edit.gdc-reader/write-character-file character (.getCanonicalPath savepath))))

(defn write-handler
  [[input tokens]]

  (if (not (character-loaded?))
    (println "Don't have a character loaded yet!")

    (write-character-file @globals/character)))


(defn load-db
  []

  (let [[localization-load-time localization-table]
        (u/timed
         (arc-reader/load-localization-table (dirs/get-localization-filepath)))

        [db-load-time db]
        (u/timed
         (arz-reader/load-game-db (dirs/get-db-filepath)
                                  localization-table))]

    ;; (println (count localization-table)
    ;;          "localization strings loaded in"
    ;;          (format "%.3f" (utils/nanotime->secs localization-load-time))
    ;;          "seconds")

    ;; (println (count db)
    ;;          "records loaded in"
    ;;          (format "%.3f" (utils/nanotime->secs db-load-time))
    ;;          "seconds")

    ;; (println)
    ;; (println "Ready to rock!")
    ;; (println)

    db
  ))

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

(defn- is-valid-item?
  [item]

  (if (or  (nil? (:basename item)) (empty? (:basename item)))
    false
    true))

(defn- item-name
  [item db]

  (if-not (is-valid-item? item)
    nil

    (let [related-records (related-db-records item db)
          base-record (-> (filter #(= (:basename item) (:recordname %1)) related-records)
                          (first))

          base-name (or (get base-record "itemNameTag") (-> (get base-record "description")
                                                            (string/replace "^k" "")))

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
          (let [prefix-name (-> (filter #(string/includes? (:recordname %1) "/prefix/") related-records)
                                (first)
                                (get "lootRandomizerName"))
                suffix-name (-> (filter #(string/includes? (:recordname %1) "/suffix/") related-records)
                                (first)
                                (get "lootRandomizerName"))
                quality-name (base-record "itemQualityTag")
                ]

            (->> [prefix-name quality-name base-name suffix-name]
                 (filter #(not (nil? %1)))
                 (string/join " "))
            ))
        ))))

(defn- is-item?
  "Does the given collection look like something that represents an in-game item?"
  [coll]

  (and (associative? coll)
       (contains? coll :basename)))

(defn- show-item
  "Print all related records for an item"
  [item]

  (cond

    (not (is-item? item))
    (println "Sorry, this doesn't look like an item")

    (empty? (:basename item))
    (do
      (println "This isn't a valid item (no basename)")
      (print-map item))

    :else
    (let [related-records (related-db-records item @gd-edit.globals/db)
          name (item-name item @gd-edit.globals/db)
          ]
      (when (not (nil? name))
        (println (yellow name))
        (newline))
      (print-map item :skip-item-count true)
      (newline)
      (print-result-records related-records)
      )))

(defn- affix-record-get-name
  [affix-record]
  (affix-record "lootRandomizerName"))

(defn- item-base-record-get-name
  [item-base-record]

  (let [base-name (or (get item-base-record "itemNameTag") (get item-base-record "description"))
        base-name (string/replace base-name "^k" "")

        quality-name (item-base-record "itemQualityTag")]

    (->> [quality-name base-name]
         (filter #(not (nil? %1)))
         (string/join " "))))

(defn item-or-affix-get-name
  [record]

  (or (affix-record-get-name record) (item-base-record-get-name record)))

(defn name-idx-best-matches
  [name-idx target-name]

  (->> name-idx

       ;; Score and sort the item name index
       ;; First, we rank by the name's overall similiarity
       ;; This should help to filter out items that are not at all similar
       (map (fn [[item-name item-record]]
              [(u/string-similarity (string/lower-case item-name) (string/lower-case target-name)) item-name item-record]))
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
                        (sort-by first >)
                        (first))]

    ;; Does the best match seem "good enough"?
    (if (and (not (nil? best-match))
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
                              (string/lower-case target-name)
                              (string/lower-case (or (item-name (item-def->item %1) "") @globals/db)))
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


(defn build-item-name-idx
  [db]
  (let [
        item-records (filter (fn [record]
                               (some #(string/starts-with? (:recordname record) %1)
                                     #{"records/items/gearaccessories/"
                                       "records/items/gearfeet/"
                                       "records/items/gearhands/"
                                       "records/items/gearhead/"
                                       "records/items/gearlegs/"
                                       "records/items/gearrelic/"
                                       "records/items/gearshoulders/"
                                       "records/items/geartorso/"
                                       "records/items/gearweapons/"
                                       "records/items/materia/"
                                       "records/items/faction/"}))
                             db)

        item-name-idx (group-by #(item-base-record-get-name %1) item-records)]
    item-name-idx
    ))

(defn construct-item
  [target-name db level-cap]

  ;; Build a list of base item names with their quality name
  ;; The index takes the form of: item-name => [item-records]
  ;; Multiple records may have the same name, but differ in strength
  (let [item-name-idx (build-item-name-idx @globals/db)

        ;; Build a prefix index
        prefix-records (->> db
                            (filter #(string/starts-with? (:recordname %1) "records/items/lootaffixes/prefix/"))
                            (filter #(-> (:recordname %1)
                                         (string/split #"/")
                                         (count)
                                         (= 5))))

        prefix-name-idx (group-by #(%1 "lootRandomizerName") prefix-records)

        ;; Build a suffix index
        suffix-records (->> db
                            (filter #(string/starts-with? (:recordname %1) "records/items/lootaffixes/suffix/"))
                            (filter #(-> (:recordname %1)
                                         (string/split #"/")
                                         (count)
                                         (= 5))))

        suffix-name-idx (group-by #(%1 "lootRandomizerName") suffix-records)


        ;; Try to decompose the complete item name into its prefix, base, and suffix
        analysis (analyze-item-name item-name-idx prefix-name-idx suffix-name-idx target-name)

        ;; Now that we know what the item is composed of, try to bring up the strength/level of each part
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
      (item-def->item results))))

(defn set-item-handler
  [[input [path target-name level-cap-str]]]

  (let [path-keys (string/split path #"/")
        result (walk-structure @gd-edit.globals/character path-keys)
        {:keys [status found-item actual-path]} result

        level-cap (if-not (nil? level-cap-str)
                    (Integer/parseInt level-cap-str)
                    (:character-level @globals/character))
        ]

    (cond
      (= status :not-found)
      (println "The path does not specify an item")

      (= status :too-many-matches)
      (print-ambiguous-walk-result result)

      (not (contains? found-item :basename))
      (println "Something was found at the path, but it does not look like an item")

      :else
      ;; Try to construct the requested item
      (let [item (construct-item target-name @globals/db level-cap)]
        ;; If construction was not successful, inform the user and abort
        (cond
          (nil? item)
          (println "Sorry, the item could not be constructed")

          (not (> (u/string-similarity (string/lower-case target-name) (string/lower-case (item-name item @globals/db))) 0.8))
          (do
            (show-item item)
            (println "Sorry, the item generated doesn't look like the item you asked for.")
            (println (red "Item not altered")))

          ;; Otherwise, put the item into the character sheet
          :else
          (do
            (swap! globals/character assoc-in (:actual-path result)
                   (merge (get-in @globals/character actual-path) item))

            ;; Show the user what was constructed and placed
            (show-item (get-in @globals/character actual-path))))
        ))))

(def command-help-map
  [["exit"  "Exits the program"]
   ["q"     "Query the database"
    (string/join "\n"
                 ["Query the database using a series of conditions in the form of"
                  "  <partial fieldname> <operator> <value>"
                  ""
                  "<partial fieldname> can also be a special keywords:"
                  " 'recordname' match against the name/path of the db record"
                  " 'key' match some key/fieldname in the record"
                  " 'value' match some value entry in the record"
                  ""
                  "valid operators are: ~ = != < > <= >="
                  " ~ is used for partial string matches"
                  ""
                  "Example 1:"
                  " q recordname~weapons"
                  ""
                  " This this retrieves where 'weapons' is part of the recrodname."
                  " Since weapon db records seem to reside at 'records/items/gearweapons'"
                  " We can try partial matches against any record that have 'weapons'"
                  " in the record name."
                  ""
                  "Example 2:"
                  " q recordname~weapons value~legendary"
                  ""
                  " This find all weapons of 'legendary' quality by adding an additional"
                  " condition that we're looking for records with any field value that"
                  " partially matches the word 'legendary'"
                  ""
                  "Example 3:"
                  " q recordname~weapons value~legendary levelreq < 50"
                  ""
                  " The new clause 'levelreq < 50' means that we're looking for records"
                  " with a fieldname that partially matches 'levelreq' and where the value"
                  " of that field is '< 50'"
                  ""
                  "Example 4:"
                  " q recordname~weapons/blunt1h order offensivePhysicalMax"
                  ""
                  " Normally, the returned results are ordered by their recordname. However, you"
                  " can ask the editor to order the results by any field you want."
                  ])]

   ["qshow" "Show the next page in the query result"]
   ["qn"    "Alias for qshow"]

   ["db"    "Explore the database interactively"
    (string/join "\n"
                 ["Syntax: db <partially matched path>"
                  ""
                  "The query command helps locate exact db records if you know what you're"
                  "looking for. But if you don't know what's in the db to begin with, it will"
                  "be difficult to come up with a useful query."
                  ""
                  "This command lets you explore the db interactively instead of querying"
                  "against it."
                  ""
                  "Try the following commands:"
                  " db"
                  " db record/items"
                  " db r/item/weapons/melee/d011_blunt2h.dbr"
                  ""
                  "If supplied with a path that indicates a 'directory', the results will show you"
                  "all items residing in that directory. If the supplied path indicates a db record"
                  "it will show you the contents of that record."
                  ""
                  "Lastly, each 'component' in the path can be partially matched, so you don't have"
                  "to enter the exact path all the time. It works as long as you supply enough of"
                  "the component to uniquely identify a directory or record."
                  ])]

   ["show"  "Explore the save file data"
    (string/join "\n"
                 ["Syntax: show <partially matched path>"
                  ""
                  "This command is used to examine variables in the loaded character save file."
                  ""
                  "Example 1:"
                  " show"
                  ""
                  " This shows a top level overview of all fields in the save file."
                  ""
                  "Example 2:"
                  " show level"
                  ""
                  " This filters for fields that contains the world 'level'"
                  ""
                  "Example 3:"
                  " show equipment"
                  ""
                  " If the filter narrows down matches to a single item, the contents of that"
                  " item is displayed. In this case, the editor will show you all the contents"
                  " of items you currently have equipped."
                  ""
                  "Example 4:"
                  " show equipment/0"
                  ""
                  " The 'equipment' field is a collection of 12 items. This command says to show"
                  " only the first item. When display an item, the editor will show the full name"
                  " of the item as well as any related db records."
                  ])]

   ["set"   "Set fields in the save file"
    (string/join "\n"
                 ["Syntax: set <partially matched path to field> <new value>"
                  ""
                  "This command is used to alter field values in the loaded character save file."
                  "Any fields that can be found using the 'show' command can be altered this way."
                  ""
                  "Example 1:"
                  " set character-level 64"
                  ""
                  " This sets the character level to 64."
                  ""
                  "Example 2:"
                  " set stash-items/0/stack-count 100"
                  ""
                  " Set the first item in your stash to a stack of 100."
                  ""
                  "Example 3:"
                  " set weaponsets/0/items/0 \"legion warhammer of valor\""
                  ""
                  " Usually, the set command just puts the specified new value into the field"
                  " specified by the partial path. So it's possible to change an item's basename,"
                  " prefix, suffix, or any other field directly."
                  ""
                  " However, in the case of dealing with an item, it's actually very painful to"
                  " query for the record to use as a basename, then repeat for the prefix and the"
                  " suffix. So in the case where the partial path points at an item, it's possible"
                  " to just supply the name of the item you want. The editor will try its best to"
                  " figure the right combination of basename, prefix, and suffix. It also takes"
                  " into consideration of the character that has currently been loaded."
                  " "
                  ])]

   ["load"  "Load from a save file"]
   ["write" "Writes out the character that is currently loaded"]
   ])

(defn help-handler
  [[input tokens]]

  (cond
    ;; If there are no other parameters,
    ;; show the list of commands and a short help text.
    (= 0 (count tokens))
    (let [command-names (map #(first %1) command-help-map)
          max-name-length (reduce (fn [max-length item]
                                    (if (> (count item) max-length)
                                      (count item)
                                      max-length)
                                    )
                                  0 command-names)]
      ;; Print the name of the command followed by the short help text
      (doseq [help-item command-help-map]
        (println (format (format "%%-%ds     %%s" max-name-length) (first help-item) (second help-item)))
        )
      (newline)
      (println (string/join "\n"
                            ["To more detailed help text on a command, run: "
                             " help <command>"])))

    :else
    ;; If the user is looking for help text on a specific command,
    ;; try to find the help text.
    ;; Display the help text (or lack of one) as appropriate
    (let [command (string/join " " tokens)
          help-item (->> command-help-map
                         (filter #(= command (first %1)))
                         (first))]
      (cond
        (nil? help-item)
        (println (format "Unknown command '%s'" command))

        (> (count help-item) 2)
        (println (nth help-item 2))

        :else
        (println (format "Sorry, '%s' has no detailed help text." command))
        ))))

#_(help-handler [nil []])

#_(write-handler [nil nil])

#_(choose-character-handler [nil nil])
#_(show-handler [nil nil])
#_(show-handler [nil ["shrines/0"]])
#_(show-handler [nil ["stash-items"]])
#_(show-handler [nil ["stash/50"]])
#_(show-handler [nil ["stash-items/50"]])
#_(show-handler [nil ["stash-items/50/basename"]])
#_(show-handler [nil ["skillsets"]])

#_(set-handler [nil ["skillpoints" "12"]])
#_(set-handler [nil ["stashitems/50/basename" "123"]])

#_(let [is-set-item (some #(contains? %1 "itemSetName") r)]
    is-set-item)


#_(def r (construct-item "Skull Fetish" @gd-edit.globals/db))
#_(show-item r)
#_(show-handler [nil ["inv/0/items"]])
#_(set-item-handler  [nil ["inv/0/items/0" "legion warhammer of valor" "64"]])
#_(set-handler  [nil ["weaponsets/0/items/0" "Infernal Brimstone"]])

#_(write-handler  [nil nil])
#_(run-query "recordname~gearweapons value~legendary levelreq <= 65")
