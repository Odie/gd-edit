(ns gd-edit.command-handlers
  (:require [gd-edit.db-query :as query]
            [gd-edit.globals :as g]
            [gd-edit.utils :as u]
            [gd-edit.game-dirs :as dirs]
            [gd-edit.gdc-reader :as gdc]
            [jansi-clj.core :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [gd-edit.globals :as globals]))

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

  (reset! g/query-state (assoc @g/query-state
                               :query-string query-string
                               :result new-result
                               :page 0)))


(defn print-result-records
  [results]
  (doseq [kv-map results]
    (println (:recordname kv-map))
    (doseq [[key value] (->> kv-map
                             seq
                             (filter #(not (keyword? (first %1))))
                             (sort-by first))]

      (println (format "\t%s: %s" key (yellow value))))

    (println)))


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

  (paginate-next! g/query-state)
  (print-paginated-result @g/query-state))


(defn run-query
  [input]

  (let [predicates (try (query/query-string->query-predicates input)
                        (catch Throwable e (println (str "Query syntax error: " (.getMessage e)))))]

    (when (not (nil? predicates))

      ;; Run a query against the db using generated predicates
      (set-new-query-result! g/query-state
                             (query/query @g/db predicates)
                             input)

      (print-paginated-result @g/query-state))))

(defn query-comand-handler
  [[input tokens]]

  (if (empty? input)
      (println
"usage: q <target> <op> <value>
       <target> can be \"recordname\", \"value\", or \"key\"")
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

  (let [path (first tokens)]
    (if (nil? path)
      (println "Please provide some path to attempt matching")

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
            (println (count sorted-matches) " matches")))))))

(defn- last-path-component
  [path]

  (last (clojure.string/split path #"[/\\]")))

(defn- character-selection-screen
  []

  ;; grab a list save directories where a "player.gdc" file exists
  (let [save-dirs (->> (dirs/get-save-dir)
                       (io/file)
                       (.listFiles)
                       (filter #(.isDirectory %1))
                       (filter #(.exists (io/file %1 "player.gdc"))))]

    (reset! gd-edit.globals/menu
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
                                       (reset! globals/character
                                               (gdc/load-character-file (.getPath (io/file dir "player.gdc"))))
                                       (reset! globals/menu {})
                                       )])))
                          []
                          (map-indexed vector save-dirs))
             })))

(defn character-selection-screen! [] (reset! gd-edit.globals/menu (character-selection-screen)))

(defn choose-character-handler
  [[input tokens]]

  character-selection-screen!)

(defn- character-loaded?
  []
  (and (not (nil? @gd-edit.globals/character))
       (not (empty? @gd-edit.globals/character))))

(defn- keyword->str
  [k]
  (subs (str k) 1))

(defn- without-meta-fields
  [kv-pair]

  (not (.startsWith (str (first kv-pair)) ":meta-")))

(defn print-character
  [character-map]

  ;; We want to be able to show a nice, easy to read list...
  ;; To do this, we first need to figure out how long the keyname is
  ;; Once we determine this, we can pad the keynames.
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
         "[collection]"

         (and (string? value) (empty? value))
         "\"\""

         :else
         (yellow value))))

    (println " ")
    (println (format (format "%%%dd" (+ max-key-length 2)) (count character)) "items")))


(defn- partially-match-fields
  [path coll]

  (->> coll
       (filter without-meta-fields)
       (filter (fn [[key value]]
                 (-> key
                     (keyword->str)
                     (u/ci-match path))))
       ))

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
    (print-character @globals/character)

    :else
    (do
      (let [path (first tokens)]
        ;; (partially-match-fields path @globals/character)))))
        (print-character (partially-match-fields path @globals/character))))))


#_(choose-character-handler [nil nil])
#_(show-handler [nil ["level"]])
