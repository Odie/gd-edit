(ns gd-edit.command-handlers
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data]
            [gd-edit
             [arc-reader :as arc-reader]
             [arz-reader :as arz-reader]
             [db-query :as query]
             [game-dirs :as dirs]
             [gdc-reader :as gdc]
             [globals :as globals]
             [utils :as u]
             [self-update :as su]
             [equation-eval :as eq]
             [stack :as stack]
             [structure-walk :as sw]
             [db-utils :as dbu]]
            [gd-edit.commands.item :as item-commands]
            [jansi-clj.core :refer :all]

            [clojure.string :as str]
            [gd-edit.utils :as utils]
            [me.raynes.fs :as fs]

            [gd-edit.printer :as printer]
            [clojure.java.shell :refer [sh]]
            [taoensso.timbre :as t]
            [gd-edit.jline]))

(declare load-db-in-background build-db-index clean-display-string item-name)


;;--------------------------------------------------------------------
;; Query and pagination functions
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

(defn print-paginated-result
  [query-state]

  (let [{:keys [result page pagination-size]} query-state
        paginated-result (get-paginated-result result query-state)
        start-entry (* page pagination-size)
        end-entry (+ start-entry (min (count paginated-result) pagination-size))]

    (printer/print-result-records paginated-result)
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

(defn query-command-handler
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
               (first tokens))

        path-components (string/split path #"/")

        ;; Grab a list of items from the db that partially matches the specified path
        matches (->> path
                     (db-partial-record-path-matches @gd-edit.globals/db)

                     ;; Filter out all results that have too few path components
                     (filter (fn [item]
                               (>= (count (string/split item #"/")) (count path-components))
                               )))

        sorted-matches
        (->> matches

             ;; Consolidate the search so we only have n+1 path components
             (reduce (fn [accum item]
                       (conj accum
                             (string-first-n-path-components (inc (count (clojure.string/split path #"/"))) item)))
                     #{})

             ;; Sort the matches
             (sort))

        exact-match (->> matches
                         (some #(if (= path %)
                                  %)))
        ]

    ;; It's possible for the user to target a single record using "db show".
    ;; It that happens, we should just display the contents of that record instead of a
    ;; list of matched paths
    ;; Try to fetch the full recordname of the match now
    (if (or (= 1 (count sorted-matches))
            (some? exact-match))
      (printer/print-result-records
       [(dbu/record-by-name (first sorted-matches))])


      ;; If we have multiple record matches...
      ;; Just print the names
      (do
        (doseq [item sorted-matches]
          (println item))
        (println)
        (println (count sorted-matches) " matches")))))

(declare load-character-file
         write-character-file
         write-loaded-character-file!
         character-selection-screen!
         character-manipulation-screen!
         write-handler)

(defn- character-manipulation-screen
  []

  {:display-fn
   (fn []
     (println "Character: " (:character-name @globals/character)))

   :choice-map [["r" "reload" (fn [] (load-character-file (@globals/character :meta-character-loaded-from)))]
                ["w" "write" (fn[] (write-handler [nil]))]]})

(defn- is-cloud-save?
  [dir]
  (not (nil? (some #(string/starts-with? dir %)
                   (map #(.getParent (io/file %)) (dirs/get-steam-cloud-save-dirs))))))

(defn- is-mod-save?
  [dir]

  (let [path-components (string/split (str dir) #"[/\\]")
        length (count path-components)]
    (if (and (u/case-insensitive= (path-components (- length 3)) "save")
             (u/case-insensitive= (path-components (- length 2)) "user"))
      true
      false)))

(defn- save-dir-type
  [dir]

  (let [cloud? (is-cloud-save? dir)
        custom? (is-mod-save? dir)
        builder (StringBuilder.)]
    (if cloud?
      (.append builder "cloud")
      (.append builder "local"))
    (when custom?
      (.append builder " custom"))
    (.toString builder)))

(defn- character-selection-screen
  []

  ;; grab a list save directories where a "player.gdc" file exists
  (let [save-dirs (dirs/get-all-save-file-dirs)]

    {:display-fn
     (fn []
       (if (empty? save-dirs)
         (println (red "No save files found"))
         (println "Please choose a character to load:")))

     ;; generate the menu choices
     ;; reduce over save-dirs with each item being [index save-dir-item]
     :choice-map (reduce
                  (fn [accum [idx dir]]
                    (let [display-idx (inc idx)]

                      (conj accum
                            [(str display-idx)                    ; command string
                             (format "%s (%s save)"
                                     (u/last-path-component (.getPath dir))
                                     (save-dir-type dir))         ; menu display string
                             (fn []                               ; function to run when selected
                               (let [savepath (.getPath (io/file dir "player.gdc"))]
                                 (println "Loading from:")
                                 (u/print-indent 1)
                                 (println (yellow savepath))
                                 (load-character-file savepath)
                                 (character-manipulation-screen!))
                               )])))
                  []
                  (map-indexed vector save-dirs))
     }))

(defn- mod-selection-screen
  []

  (let [gamedir (dirs/get-game-dir)
        moddir (io/file gamedir "mods")
        mods (.listFiles moddir)]

    (if (or (not (.exists moddir))
            (empty? mods))
      (println "No mods installed")

      {:display-fn nil
       :choice-map (reduce
                    (fn [accum [idx moddir]]
                      (let [display-idx (inc idx)]
                        (conj accum
                              [(str display-idx)
                               (str (u/last-path-component (str moddir)))
                               (fn []
                                 ;; Update the global state and save the settings file
                                 (swap! globals/settings assoc :moddir (str moddir))

                                 ;; Reload the database
                                 (load-db-in-background)

                                 (stack/pop! globals/menu-stack)
                                 )])))
                    []
                    (map-indexed vector mods))})))


(defn character-selection-screen! [] (stack/replace-last! gd-edit.globals/menu-stack (character-selection-screen)))
(defn character-manipulation-screen! [] (stack/replace-last! gd-edit.globals/menu-stack (character-manipulation-screen)))

(defn choose-character-handler
  [[input tokens]]

  (reset! globals/character (empty @globals/character))
  (character-selection-screen!))

(defn- character-loaded?
  []
  (and (not (nil? @gd-edit.globals/character))
       (not (empty? @gd-edit.globals/character))))

(defn- without-recordname
  [[key value]]
  (not= key :recordname))




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
    (printer/print-map @globals/character)

    :else
    (do
      ;; Split a path into components.
      ;; We're going to use these as keys to navigate into the character sheet
      (let [path-keys (string/split (first tokens) #"/")
            base-obj (if-not (> (count path-keys) 1)
                        @globals/character

                        ;; If the user supplied a long path, we should try to walk the
                        ;; data structure first and arrive at a base object
                        (let [result (sw/walk @globals/character (butlast path-keys))
                              {:keys [status found-item]} result]

                          (cond
                            (= status :not-found)
                            (println "No matches found")

                            (= status :too-many-matches)
                            (sw/print-ambiguous-walk-result result)

                            (= status :cannot-traverse)
                            (print-cannot-traverse-walk-result result)

                            :else
                            (:found-item result)))
              )]

        ;; Now that we've found a base object to navigate into...
        (when-not (nil? base-obj)
          ;; Walk into it one more level...
          (let [result (sw/walk base-obj [(last path-keys)])
                {:keys [status]} result

                matched-obj (cond
                              (= status :not-found)
                              (println "No matches found")

                              (= status :too-many-matches)
                              (into {} (:ambiguous-matches result))

                              (= status :cannot-traverse)
                              (let [result (sw/walk @globals/character path-keys)]
                                (print-cannot-traverse-walk-result result))

                              ;; If we found a result and it looks like a primitive...
                              ;; We want to print the path to that primitive then the value
                              (and (= status :found) (dbu/is-primitive? (:found-item result)))
                              (let [result (sw/walk @globals/character path-keys)]
                                (println (->> (:actual-path result)
                                              (map u/keyword->str)
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
              (printer/print-object matched-obj))))))))


(defn- parseBoolean
  [val-str]

  (let [true-aliases ["true" "t" "1"]
        false-aliases ["false" "f" "0"]]
    (cond
      (contains? (into #{} true-aliases) val-str)
      true

      (contains? (into #{} false-aliases) val-str)
      false

      :else
      (throw (Throwable.
              (str/join "\n"
                        [(format "Can't interpret \"%s\" as a boolean value" val-str)
                         "Try any of the following: "
                         (str "  true  - " (str/join ", " true-aliases))
                         (str "  false - " (str/join ", " false-aliases))]))))))


(defn coerce-str-to-type
  "Given an value as a string, return the value after coercing it to the correct type."
  [val-str type]

  (cond
    ;; If the caller asked to convert the value to a string,
    ;; we don't have to do anything because the value should already be a string
    (= java.lang.String type)
    val-str

    (= java.lang.Float type)
    (Float/parseFloat val-str)

    (= java.lang.Double type)
    (Double/parseDouble val-str)

    (= java.lang.Short type)
    (Short/parseShort val-str)

    (= java.lang.Integer type)
    (Integer/parseInt val-str)

    (= java.lang.Long type)
    (Long/parseLong  val-str)

    (= java.lang.Boolean type)
    (parseBoolean val-str)))

(defn coerce-number-to-type
  [val-number to-type]

  (cond
    (= java.lang.String to-type)
    (.toString val-number)

    (= java.lang.Float to-type)
    (float val-number)

    (= java.lang.Double to-type)
    (double val-number)

    (= java.lang.Short type)
    (Short. val-number)

    (= java.lang.Integer to-type)
    (int val-number)

    (= java.lang.Long to-type)
    (long val-number)

    (= java.lang.Boolean to-type)
    (if (= val-number 0)
      false
      true)

    :else
    (throw (Throwable. (format "Don't know how to coerce %s => %s"
                               (str (type val))
                               (str to-type))))))

(defn coerce-to-type
  [val to-type]
  (cond
    (string? val)
    (coerce-str-to-type val to-type)

    (number? val)
    (coerce-number-to-type val to-type)

    :else
    (throw (Throwable. (format "Don't know how to coerce %s => %s"
                               (str (type val))
                               (str to-type))))))

(defn coerce-map-numbers-using-reference
  "Coerce all map values that are numbers to the same type as the field in the reference map."
  [m reference]

  (->> m
       (map (fn [[k v]]
              (if (number? v)
                [k (coerce-to-type v (type (reference k)))]
                [k v])))
       (into (empty m))))

(defn- find-item-component-by-name
  [db component-name]

  (->> db
       (filter (fn [record]
                 (and
                  (u/case-insensitive-match (:recordname record) "items/materia")
                  (u/case-insensitive-match (record "description") component-name))))
       (first)))

(defn- set-item--relic-name
  [db update-path newval]

  ;; An item can have a relic/component attached
  ;; To make the program a bit more friendly to use, the user can set the relic-name to a
  ;; the name of a record, or just the English display name of a relic/component.
  (let [component-record (find-item-component-by-name db newval)]
        (cond

          ;; Does the component name look like a display name of a component?
          component-record
          (reset! globals/character
                  (update-in @globals/character update-path (fn [oldval] (:recordname component-record))))


          ;; Otherwise, we just set
          :else
          (swap! globals/character update-in update-path (fn [oldval] newval)))))

(defn set-character-field
  [character path newval]

  (let [;; What's the current value at that location?
        value (get-in character path)

        ;; What is the type of the value? We'll have to coerce the supplied new value
        ;; to the same type first.
        newval (try (coerce-to-type newval (type value))
                    (catch Exception e :failed))]

    ;; Set the new value into the character sheet
    (update-in character path (fn [oldval] newval))))

(defn path-sibling
  "Given a path as a vector, return a path to the 'sibling'."
  [path sibling-field]

  (conj (->> path
             (butlast)
             (into []))
        sibling-field))

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
            walk-result (sw/walk @globals/character path-keys)
            {:keys [status]} walk-result]

        (cond
          (= status :not-found)
          (println "No matches found")

          (= status :too-many-matches)
          (sw/print-ambiguous-walk-result walk-result)

          (= status :found)
          (do
            ;; We found the value the user wanted to update
            (let [value (:found-item walk-result)
                  ;; What is the type of the value? We'll have to coerce the supplied new value
                  ;; to the same type first.
                  newval (try (coerce-to-type (second tokens) (type value))
                              (catch Exception e :failed))
                  val-path (:actual-path walk-result)]

              (cond
                ;; Is the targetted value an item?
                ;; Let the set-item handler deal with it
                (dbu/is-item? value)
                (item-commands/set-item-handler [input tokens])

                ;; Did the user specify the some inventory items collection?
                (and (= (first val-path) :inventory-sacks)
                     (= (last val-path) :inventory-items))
                (item-commands/set-item-handler [input tokens])

                (and
                 (dbu/is-item? (get-in @globals/character (butlast val-path)))
                 (= :relic-name (last val-path)))
                (do
                  (set-item--relic-name @globals/db val-path newval)
                  (swap! globals/character
                         set-character-field
                         ;; Set the item's relic-completion-level field...
                         (path-sibling val-path
                                       :relic-completion-level)

                         ;; to 4
                         4))


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
                        (update-in @globals/character val-path (fn [oldval] newval)))
                )
              nil
              ))

          :else
          (throw (Throwable. "Unhandled case")))))))

(defn- get-loadpath
  [character]
  (:meta-character-loaded-from character))

(defn- get-savepath
  [character]

  (let [loadpath (get-loadpath character)
        loadpath-components (u/filepath->components (str loadpath))]

    (u/components->filepath
     (assoc loadpath-components
            (- (count loadpath-components) 2)
            (str "_" (:character-name character))))))

(defn- get-new-backup-path
  [filepath]

  ;; Grab all sibling files of the given file
  (let [sibling-files (-> (io/file filepath)
                          (.getParentFile)
                          (.listFiles))

        ;; What's the filename?
        filename (u/last-path-component filepath)

        ;; Filter for all items that starts with the filename followed by .bakXXX
        save-backups (->> sibling-files
                          (filter #(.isFile %1))
                          (filter #(string/starts-with? (.getName %1) filename))
                          (filter #(re-matches #"\.bak.*$" (subs (.getName %) (count filename)))))]

    (format "%s.bak%d" filepath (inc (count save-backups)))))

(defn- backup-file
  "Renames the given file to a .bakXXX file if it exists."
  [filepath]

  (let [f (io/file filepath)
        backup-path (get-new-backup-path filepath)]

    (if (.exists f)
      [(.renameTo f (io/file backup-path)) backup-path]
      [:nothing-to-backup backup-path])))

(defn load-character-file
  [savepath]

  (reset! globals/character
          (gdc/load-character-file savepath))
  (reset! globals/last-loaded-character @globals/character))

(defn- write-character-file-after-backup
  [character]

  (let [savepath (get-savepath character)

        ;; save the file
        [backup-status backup-path] (backup-file savepath)]
    (cond
      (= backup-status true)
      (do
        (println "Save file backed up to:")
        (u/print-indent 1)
        (println (yellow backup-path)))

      (= backup-status false)
      (do
        (println "Cannot backup file up to:")
        (u/print-indent 1)
        (println (yellow backup-path))))

    (println "Saving file:" )
    (u/print-indent 1)
    (println (yellow savepath))

    (gd-edit.gdc-reader/write-character-file character (.getCanonicalPath (io/file savepath)))))

(defn- rename-save-dir-if-required
  [character]

  (let [loadpath (get-loadpath character)
        savepath (get-savepath character)]

    (if (= loadpath savepath)
      :rename-not-needed

      (if (.renameTo (-> (io/file loadpath)
                             (.getParentFile))
                         (-> (io/file savepath)
                             (.getParentFile)))
        :rename-success
        :rename-failed))))

(defn- write-character-file
  [character]

  (let [character-name (:character-name character)

        ;; If the save directory should be renamed (because character name changed),
        ;; do so now...
        rn-status (rename-save-dir-if-required character)]

    ;; Print out any rename status
    (cond
      (= rn-status :rename-success)
      (println "Renamed save directory to match character name: " character-name)

      (= rn-status :rename-failed)
      (do
        ;; Did the rename fail because there because the target directory already exists?
        ;; If so, inform the user...
        (if (.exists (.getParentFile (io/file (get-savepath character))))
          (do
            (println "Unable to rename save directory because it conflicts with an existing directory: ")
            (u/print-indent 1)
            (println (yellow (.getCanonicalPath (.getParentFile (io/file (get-savepath character))))))
            (newline)
            (println "Please rename your character before trying again."))

          ;; If we don't know why the renamed failed, just print a generic message.
          (println "Unable to rename save directory to match character name: " character-name))))

    ;; If rename failed (rename required, but could not be done...)
    ;; Don't write the file and just return the character as is
    (if (= rn-status :rename-failed)
      character

      ;; Otherwise, actually write out the character file
      ;; Update the last loaded location in case it changed
      (do
        (write-character-file-after-backup character)
        (assoc character :meta-character-loaded-from (get-savepath character))))))

(defn write-loaded-character-file!
  []

  ;; Although we're writing out the file...
  ;; Some meta-data on the character may change due to character & savedir rename handling
  (reset! globals/character (write-character-file @globals/character)))

(defn write-separate-copy
  [character new-name]

  (let [from-char-dir-file (-> (get-savepath character)
                               (io/file)
                               (.getParentFile))

        modified-character (assoc character :character-name new-name)
        to-char-dir-file (-> (get-savepath modified-character)
                             (io/file)
                             (.getParentFile))]

    (if (.exists to-char-dir-file)
      [:new-path-already-exists (str to-char-dir-file)]

      (do
        ;; Recursively copy the original character directory
        (fs/copy-dir from-char-dir-file to-char-dir-file)

        ;; Write out the character to the new location
        (gd-edit.gdc-reader/write-character-file modified-character
                                                 (-> modified-character
                                                     (get-savepath)
                                                     (io/file)
                                                     (.getCanonicalPath)))
        [:done (str to-char-dir-file)]))))


(defn get-process-list
  []

  (if (u/running-windows?)
    (let [plist-csv (:out (sh "tasklist.exe" "/fo" "csv" "/nh"))
          plist (for [row (str/split-lines plist-csv)]
                  (->> row
                       (re-seq #"\"(.*?)\"")
                       (map second)
                       (zipmap [:image-name :pid :session-name :session-num :mem-usage])))]
      plist)))

(defn find-running-process
  [plist process-name]

  (filter #(= (:image-name %) process-name) plist))

(defn is-grim-dawn-running?
  []

  (let [plist (get-process-list)]
    ;; If the process list cannot be retrieved for some reason...
    (if (nil? plist)
      false ;; Assume the program isn't running

      ;; If we can't find any items in the process list named grim dawn...
      (if (empty? (find-running-process plist "Grim Dawn.exe"))
        false ;; The program isn't runing...
        true))))

(defn write-handler
  [[input tokens]]

  (if (not (character-loaded?))
    (println "Don't have a character loaded yet!")

    ;; Make sure GD isn't running
    (if (is-grim-dawn-running?)
      (do
        (println "Please quit Grim Dawn before saving the file.")
        (println (red "File not saved!")))

      ;; If the user invoked the command without any parameters...
      (if (nil? (first tokens))

        ;; Write out the currently loaded character
        (write-loaded-character-file!)

        ;; The user supplied some parameter with the command...
        ;; Try to write out a new copy of the character
        (let [[status savepath] (write-separate-copy @globals/character (first tokens))]
          (cond
            (= status :new-path-already-exists)
            (do
              (println (red "Cannot copy character because the directory already exists:"))
              (u/print-indent 1)
              (println savepath))

            :else
            (do
              (println (green "Ok!"))
              (newline)
              (println "If you're running steam with cloud saves, please remember to restart steam.")
              (println "Otherwise, your copied character will not show up in the character selection menu.")
              )))))))



(defn mod-db-file
  "Returns the configured mod's db file as a java.lang.File object or nil"
  []
  (let [mod-dir (:moddir @globals/settings)]
    (when (and mod-dir (.exists (dirs/make-db-filepath mod-dir)))
      (dirs/make-db-filepath mod-dir))))

(defn dlc-db-file
  "Returns the dlc db file as a java.lang.File object or nil"
  []
  (when-let [dlc-dir (io/file (dirs/get-game-dir) "gdx1")]
    (let [dlc-db-file (io/file dlc-dir "database" "GDX1.arz")]
      (when (.exists dlc-db-file)
        dlc-db-file))))

(defn db-files
  "Returns an array of database files to load"
  []

  (filterv some?
           [;; Grab the localization file for the base game
            (dirs/get-db-filepath)

            ;; Grab the dlc's localization file if it exists
            (dlc-db-file)

            ;; Grab the localization file for the configured mod if it exists
            (mod-db-file)]))


(defn mod-localization-file
  "Returns the configured mod's localization file as a java.lang.File object or nil"
  []
  (let [mod-dir (:moddir @globals/settings)]
    (when (and mod-dir (.exists (dirs/make-localization-filepath mod-dir)))
      (dirs/make-localization-filepath mod-dir))))

(defn dlc-localiation-file
  "Returns the dlc localization file as a java.lang.File object or nil"
  []

  (when-let [dlc-dir (io/file (dirs/get-game-dir) "gdx1")]
    (let [dlc-loc-file (dirs/make-localization-filepath dlc-dir)]
      (when (.exists dlc-loc-file)
        dlc-loc-file))))

(defn localization-files
  "Returns an array of localization files to load"
  []

  (filterv some?
           [;; Grab the localization file for the base game
            (dirs/get-localization-filepath)

            ;; Grab the dlc's localization file if it exists
            (dlc-localiation-file)

            ;; Grab the localization file for the configured mod if it exists
            (mod-localization-file)]))

(defn load-localization-files
  []
  (->> (localization-files) ;; grab all localization files we want to load
       (map arc-reader/load-localization-table) ;; load each file
       (apply merge)))

(defn load-db
  [localization-table]

  (t/debug "Entering load-db")
  (->> (db-files) ;; Grab all db files we want to load
       (map #(arz-reader/load-game-db % localization-table)) ;; load all the db files
       (map build-db-index) ;; merge all the loaded db records
       (apply merge)
       (vals)))

(defn build-db-index
  [db]

  (->> db
       (map (fn [record] [(:recordname record) record]))
       (into {})))

(defn load-db-in-background
  []

  (intern 'gd-edit.globals 'localization-table (future (u/log-exceptions (load-localization-files))))
  (intern 'gd-edit.globals 'db (future (u/log-exceptions (load-db @globals/localization-table))))
  (intern 'gd-edit.globals 'db-index (future (build-db-index @globals/db))))

(def command-help-map
  [["exit"  "Exits the program"]
   ["q"     "Query the database"
    (string/join "\n"
                 ["Query the database using a series of conditions in the form of"
                  "  <partial fieldname> <operator> <value>"
                  ""
                  "<partial fieldname> can also be one of the following special keywords:"
                  " 'recordname' matches against the name/path of the db record"
                  " 'key' matches some key/fieldname in the record"
                  " 'value' matches some value entry in the record"
                  ""
                  "valid operators are: ~ *= = != < > <= >="
                  " ~ and *= are used for partial string matches"
                  ""
                  "Example 1:"
                  " q recordname~weapons"
                  ""
                  " This says to return a list of records that has the word 'weapons' as part of"
                  " the recordname. Since weapon records seem to mostly live under"
                  " 'records/items/gearweapons', this will effectively return all weapons in the"
                  " game."
                  ""
                  "Example 2:"
                  " q recordname~weapons value~legendary"
                  ""
                  " In addition to matching against all weapons, as we saw in example 1, the added"
                  " 'value~legendary' condition will narrow the list down to only records where"
                  " some field contains the string 'legendary'."
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
                  " can ask the editor to order the results by any field you'd like ."
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
   ["write" "Writes out the character that is currently loaded"
    (string/join "\n"
                 ["Syntax: write"
                  ""
                  "Writes out the currently loaded character."
                  ""
                  "Syntax: write <new-character-name>"
                  ""
                  "Makes a copy of the currently loaded character."])]
   ["class" "Displays the classes/masteries of the loaded character"]
   ["class list" "Display classes/masteries known ot the editor"]
   ["class add" "Add a class/mastery by name"]
   ["class remove" "Remove a class/mastery by name"]
   ["savedir" "Sets the save game directory to a path"
    (string/join "\n"
                 ["Syntax: savedir <full path to save game directory>"])]
   ["savedir clear" "Removes the previous set game directory"]
   ["gamedir" "Sets the game installation directory to a path"
    (string/join "\n"
                 ["Syntax: gamedir <full path to save game installation directory>"])]
   ["gamedir clear" "Removes the previously set game installation directory"]
   ["mod" "Displays the mod currently selected"]
   ["mod pick" "Picks an installed mod to activate"]
   ["mod clear" "Unselect the currently selected mod"]
   ["level" "Set the level of the loaded character to a new value"]
   ["respec" "Respecs the loaded character"
    (string/join "\n"
                 ["Syntax: respec <respec-type>"
                  ""
                  "Valid respec types:"
                  " attributes - refund all attribute points spent"
                  " skills - remove all masteries & skills and refund skill points spent"
                  " devotions - remove all devotions and refund devotion points spent"
                  " all - combination of all of the above"
                  ])]
   ["update" "Update to the latest version of gd-edit"]
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
                             " help <command>"
                             ""
                             "Need more help? Check the docs!\n\thttps://odie.github.io/gd-edit-docs/faq/\n"
                             ])))

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


(defn- character-classes-with-index
    "Returns a set of db records that represents the classes the player has taken"
  [character]

  (->> (map-indexed vector (character :skills))
       (filter (fn [[idx skill]] (= "Skill_Mastery" (-> (:skill-name skill)
                                                        (dbu/record-by-name)
                                                        (get "Class")
                                                         ))))))

(defn- character-classes
  "Returns a set of db records that represents the classes the player has taken"
  [character]

  (map second (character-classes-with-index character)))

(defn- db-class-ui-records
  []

  (->> (dbu/record-by-name "records/ui/skills/skills_mastertable.dbr")
       ;; Grab all the fields named "skillCtrlPaneXXX"
       (filter (fn [[k v]] (string/starts-with? (str k) "skillCtrlPane")))

       ;; sort by the number behind the "skillCtrlPane" string
       (sort-by (fn [[k v]]
                  (Integer/parseInt (subs k (count "skillCtrlPane")))))

       ;; Follow the link to the classtable
       (map (fn [[k v]] (dbu/record-by-name v)))
       (filter #(not (empty? %)))))

(defn- db-class-ui-record->class-name
  [class-ui-record]
  (clean-display-string (class-ui-record "skillTabTitle")))

(defn- db-class-ui-record->class-record
  [class-ui-record]

  (-> (class-ui-record "tabSkillButtons")
      (first)

      ;; Look up the records and grab the "skillName" field
      ;; This shoud give us the _classtraining db records
      (dbu/record-by-name)
      (get "skillName")
      (dbu/record-by-name)))

(defn- db-class-records
  []

  (->> (db-class-ui-records)
       (map db-class-ui-record->class-record)))


(defn- db-class-record-by-class-name
  [classname]

  (-> (db-class-ui-records)
      (filter #())))

(defn- clean-display-string
  "Remove any strange formatting symbols from a display string"
  [s]

  (string/replace s #"\^." ""))

(defn- print-character-classes
  [character db]

  ;; Find all character "skills" that represents a class mastery

  (let [;; Generate a mapping from class record recordname => class name
        class-display-name-map (->> (db-class-ui-records)
                                    (map (fn [ui-record]
                                           [(:recordname (db-class-ui-record->class-record ui-record))
                                            (db-class-ui-record->class-name ui-record)]))
                                    (into {}))

        classes (->> (character-classes-with-index character)
                     (map (fn [[idx record]]
                            {:idx idx
                             :skill record
                             :skill-display-name (class-display-name-map (:skill-name record))
                             })))]

    ;; Print the display names
    (println "classes:")
    (if (empty? classes)
      (do
        (u/print-indent 1)
        (println (yellow "None")))

      (doseq [klass classes]
        (u/print-indent 1)
        (println (yellow (:skill-display-name klass)))
                 (format "(skills/%d)" (:idx klass)))
                 )))

(defn class-handler
  "Show the class of the loaded character"
  [[input tokens]]

  (print-character-classes @globals/character @globals/db))

(defn class-list-handler
  "Show the class of the loaded character"
  [[input tokens]]

  (println "Known classes:")
  (doseq [classname (->> (db-class-ui-records)
                         (map db-class-ui-record->class-name))]
    (u/print-indent 1)
    (println classname)))


(defn class-remove-by-name
  [character class-name]

  (let [class-display-name-map (->> (db-class-ui-records)
                                    (map (fn [ui-record]
                                           [(db-class-ui-record->class-name ui-record)
                                            (:recordname (db-class-ui-record->class-record ui-record))]))
                                    (into {}))

        skill-name-to-remove (class-display-name-map class-name)

        skill-to-remove (first (filter
                                (fn [skill]
                                  (= skill-name-to-remove (:skill-name skill)))
                                (@globals/character :skills)))]

    (if (nil? skill-to-remove)
      character
      (merge character
             {:skills (into (empty (:skills character))
                            (remove (fn [skill] (= skill skill-to-remove)) (:skills character)))
              :skill-points (+ (:skill-points character) (:level skill-to-remove))}))))


(defn class-remove-handler
  "Remove a class to the currently loaded character by partial name"
  [[input tokens]]

  (if (empty? tokens)
    (println "Please provide the partial name of the class to remove from the character")
    ;; Get the db record that represents the class mastery
    (let [class-to-remove (first tokens)

          matched-class (->> (db-class-ui-records)
                             (map db-class-ui-record->class-name)
                             (filter #(u/ci-match % class-to-remove))
                             (first))]

      (if (empty? matched-class)
        (println (format "\"%s\" doesn't match any of the known classes" class-to-remove))

        ;; Update the character
        (let [modified-character (class-remove-by-name @globals/character matched-class)]

          ;; Inform the user what happened
          (println "Removing class:" matched-class)
          (print-character-classes modified-character @globals/db)

          (println)
          (println "Updating the following fields:")
          (printer/print-map-difference (clojure.data/diff @globals/character modified-character))

          ;; Actually update the loaded character
          (swap! globals/character (constantly modified-character)))))))


(defn class-add
  [character klass]

  (merge character
         ;; Adding a mastery requires investing at least 1 skill point in it
         ;; Otherwise, the game will just ignore the choice
         {:skills (conj (:skills character)
                        {:devotion-level 0
                         :devotion-experience 0
                         :skill-active false
                         :autocast-skill-name ""
                         :skill-transition false
                         :skill-name (:recordname klass),
                         :level 1
                         :sublevel 0
                         :autocast-controller-name ""
                         :enabled true})

          ;; Deduct a skill point if possible
          :skill-points (max (dec (:skill-points character)) 0)}))

(defn prompt-yes-no
  "Prompts the user to answer Y or N, returns true or false depending on user input."
  [prompt]
  (print prompt)
  (print " ")
  (print "(Y/N) ")
  (flush)
  (let [line (str/lower-case (read-line))]
    (cond
      (str/starts-with? line "y") true
      (str/starts-with? line "n") false
      :else (do
              (println)
              (println "Please enter 'Y' or 'N'.")
              (recur prompt)))))

(defn class-add-handler
  "Add a class to the currently loaded character by partial name"
  [[input tokens]]

  (if (empty? tokens)
    (println "Please provide the partial name of the class to remove from the character")
    ;; Get the db record that represents the class mastery
    (let [klass (->> (db-class-records)
                     (filter #(u/ci-match (%1 "skillDisplayName") (first tokens)))
                     (first))]

      (if (nil? klass)
        (println (format "\"%s\" doesn't match any of the known classes" (first tokens)))

        (let [perform-op (if-not (zero? (:skill-points @globals/character))
                           true
                           (do
                             (println (str/join "\n" ["You need at least 1 skill point to add new mastery."
                                                      "Adding a new mastery now will automatically add 1 skill point to your character."]))
                             (prompt-yes-no "Really add class?")))]
          (when perform-op
            (let [modified-character (class-add @globals/character klass)]

              ;; Inform the user what happened
              (println "Adding class:" (klass "skillDisplayName"))
              (print-character-classes modified-character @globals/db)

              (println)
              (println "Updating the following fields:")
              (printer/print-map-difference (clojure.data/diff @globals/character modified-character))

              ;; Actually update the loaded character
              (swap! globals/character (constantly modified-character)))))))))


(defn load-settings-file
  "Load settings file into globals/settings"
  []

  (reset! globals/settings (or (u/load-settings) {})))

(defn setting-gamedir-clear!
  []

  (t/debug "Clearing gamedir")
  (swap! globals/settings dissoc :game-dir))

(defn setting-gamedir-set!
  [game-dir]

  (t/debug "Setting gamedir")
  (u/log-exp game-dir)

  ;; Verify that this looks like a game directory
  (if-not (dirs/looks-like-game-dir game-dir)
    ;; If this isn't a valid game directory, print an error msg and exit.
    (println (format "\"%s\" does not look like a game directory" game-dir))

    (do
      ;; If this *is* a valid game directory, set it into a global variable.
      (swap! globals/settings update :game-dir #(identity %2) game-dir)

      ;; Reload game db using the new game directory.
      (load-db-in-background))))

(defn gamedir-clear-handler
  [[input tokens]]

  (setting-gamedir-clear!)
  (println "Ok!"))

(defn- gamedir-show
  []

  (println "Currently using this as game dir:")
  (println "    " (let [game-dir  (dirs/get-game-dir)]
                    (if (nil? game-dir)
                      (red "None")
                      game-dir))))

;; TODO Use better error handling here so we're not printing "Ok!" when there's an error
(defn- gamedir-set
  [game-dir]

  (if (empty? game-dir)
    (setting-gamedir-clear!)
    (setting-gamedir-set! game-dir))
  (println "Ok!"))

(defn gamedir-handler
  [[input tokens]]

  (t/debug "Entering gamedir-handler")
  (u/log-exp input)
  (u/log-exp tokens)

  (cond
    (= 0 (count tokens))
    (gamedir-show)

    :else
    (let [game-dir (first tokens)]
      (gamedir-set game-dir))))

(defn setting-savedir-clear!
  []

  (swap! globals/settings dissoc :save-dir))

(defn setting-savedir-set!
  [save-dir]

  (swap! globals/settings update :save-dir #(identity %2) save-dir))


(defn- maybe-return-to-character-selection-screen
  []

  ;; If no character has been loaded yet, reload the screen...
  (if (empty? @globals/character)
    (choose-character-handler nil)

    (do
      (println (str/join "\n"
                         ["Savedir changed."
                          "You can use the \"load\" command to return to the character selection screen."
                          "Any unsaved changes for the current character will be lost."
                          ])))))

(defn savedir-clear-handler
  [[input tokens]]

  (setting-savedir-clear!)
  (println "Ok!")
  (maybe-return-to-character-selection-screen))

(defn savedir-handler
  [[input tokens]]

  (cond
    (= 0 (count tokens))
    (do
      (println "Currently looking through these directories for save files:")
      (doseq [loc (dirs/get-save-dir-search-list)]
        (println (str "    " loc))))

    :else
    (let [save-dir (first tokens)]

      (if (empty? save-dir)
        (setting-savedir-clear!)
        (setting-savedir-set! save-dir))
      (println "Ok!")
      (maybe-return-to-character-selection-screen))))

(defn update-handler
  [[input tokens]]

  (let [result (su/try-self-update)]
    (when (= result :up-to-date)
      (println "Already running latest version"))))

(defn mod-handler
  [[input tokens]]

  (println "currently selected mod:")
  (u/print-indent 1)
  (if (:moddir @globals/settings)
    (println (u/last-path-component (:moddir @globals/settings)))
    (println "none")))

(defn mod-pick-handler
  [[input tokens]]

  (stack/push! globals/menu-stack (mod-selection-screen)))

(defn mod-clear-handler
  [[input tokens]]

  (swap! globals/settings dissoc :moddir)

  ;; Reload the database
  (load-db-in-background)

  (println "Ok!"))


(defn attribute-points-total-at-level
  "Return the total number of skill points a character should have at the give level."
  [level]
  {:pre [(>= level 1)]}

  (let [data-record (dbu/record-by-name "records/creatures/pc/playerlevels.dbr")
        level (dec level)]

    (* level (data-record "characterModifierPoints"))))

(defn skill-points-total-at-level
  "Return the total number of skill points a character should have at the give level."
  [level]
  {:pre [(>= level 1)]}

  (let [data-record (dbu/record-by-name "records/creatures/pc/playerlevels.dbr")
        level (dec level)]

    (apply + (take level (data-record "skillModifierPoints")))))

(defn xp-total-at-level
  "Returns the number of exp points a character should have at the given level."
  [level]
  {:pre [(>= level 1)]}

  (let [data-record (dbu/record-by-name "records/creatures/pc/playerlevels.dbr")]

    (if (= level 1)
      0
      (int (eq/evaluate (data-record "experienceLevelEquation") {"playerLevel" (dec level)})))))


(defn fields-for--modify-character-level
  [character new-level]

  (coerce-map-numbers-using-reference
   {:character-level new-level
    :level-in-bio new-level
    :max-level new-level
    :experience (xp-total-at-level new-level)
    :skill-points
    (let [points-to-award
          (- (skill-points-total-at-level new-level)
             (skill-points-total-at-level (character :character-level)))]

      (max 0 (+ (:skill-points character) points-to-award)))
    :attribute-points
    (let [points-to-award
          (- (attribute-points-total-at-level new-level)
             (attribute-points-total-at-level (character :character-level)))]
      (max 0 (+ (:attribute-points character) points-to-award)))

    :masteries-allowed (max 2 (:masteries-allowed character))}

   character))

(defn modify-character-level
  [character new-level]

  (merge character (fields-for--modify-character-level character new-level)))

(defn level-handler
  [[input tokens]]

  (cond
    (empty? tokens)
    (println
     "usage: level <new-level>")

    :else
    (let [data-record (dbu/record-by-name "records/creatures/pc/playerlevels.dbr")
          level (coerce-str-to-type (first tokens) java.lang.Integer)
          level-limit (data-record "maxPlayerLevel")
          ]
      (cond
        (< level 1)
        (println "Please enter a level value that is 1 or greater")

        (> level level-limit)
        (println "Sorry, max allowed level is" level-limit)

        :else
        (let [modified-character (merge @globals/character
                                        (fields-for--modify-character-level @globals/character level))]
          (println "Changing level to" level)
          (newline)

          (println "Updating the following fields:")
          (printer/print-map-difference (clojure.data/diff @globals/character modified-character))

          (swap! globals/character modify-character-level level)))
      )))

;;------------------------------------------------------------------------------
;; Skills manipulation functions
;;

(defn skill-is-devotion?
  [skill]

  (string/starts-with? (:skill-name skill) "records/skills/devotion"))

(defn skills-vec->skills-by-category
  [skills-list]

  {:default (take 5 skills-list)
   :skills (->> (drop 5 skills-list)
                (filter #(not (skill-is-devotion? %))))

   :devotions (->> (drop 5 skills-list)
                   (filter skill-is-devotion? ))})

(defn skills-by-category->skills-vec
  [skills-by-category]

  (->> (concat (:default skills-by-category)
               (:skills skills-by-category)
               (:devotions skills-by-category))
       (into [])))

(defn respec-character-attributes
  [character]

  (let [base-attr-points 50
        data-record (dbu/record-by-name "records/creatures/pc/playerlevels.dbr")]

    (merge character
           (coerce-map-numbers-using-reference
            {:physique base-attr-points
             :cunning base-attr-points
             :spirit base-attr-points

             :attribute-points
             (let [points-to-award
                   (+
                    (/ (- (:physique character) base-attr-points)
                       (data-record "strengthIncrement"))
                    (/ (- (:cunning character) base-attr-points)
                       (data-record "dexterityIncrement"))
                    (/ (- (:spirit character) base-attr-points)
                       (data-record "intelligenceIncrement")))]
               (-> (+ (:attribute-points character) points-to-award)
                   (max 0)))}
            character))))


(defn respec-character-skills
  [character]

  (let [skills-by-category (skills-vec->skills-by-category (:skills character))
        skills (:skills skills-by-category)

        ;; Compute how many skill points were used
        points-to-award (reduce #(+ % (:level %2))
                                0
                                skills)]

    (merge character
           (coerce-map-numbers-using-reference
            {;; Refund the points used back to the character
             :skill-points (+ (:skill-points character) points-to-award)

             ;; Remove all skills from the skills vector
             :skills (skills-by-category->skills-vec
                      (dissoc skills-by-category :skills))}
            character))))

(defn respec-character-devotions
  [character]

  ;; Grab a list of devtions taken
  (let [skills-by-category (skills-vec->skills-by-category (:skills character))
        devotions (:devotions skills-by-category)]

    (merge character
           (coerce-map-numbers-using-reference
            {;; Refund 1 devotion point per devotions taken
             :devotion-points (+ (:devotion-points character) (count devotions))

             ;; Remove all devotions from the skills vector
             :skills (skills-by-category->skills-vec
                      (dissoc skills-by-category :devotions))}
            character))))

(defn respec-handler
  [[input tokens]]

  (let [mode (string/lower-case (or (first tokens) "all"))
        valid-modes #{"all" "attributes" "devotions" "skills"}]

    ;; Sanity check on the respec mode
    (if-not (contains? valid-modes mode)
      (do
        (println "Please choose from one of the valid respec types:")
        (doseq [r-type (sort valid-modes)]
          (u/print-indent 1)
          (println r-type)))

      ;; Try to modify the character according to the chosen mocd
      (let [modified-character
            (cond
              (= mode "all")
              (do
                (->> @globals/character
                     (respec-character-devotions)
                     (respec-character-skills)
                     (respec-character-attributes)))

              (= mode "attributes")
              (respec-character-attributes @globals/character)

              (= mode "skills")
              (respec-character-skills @globals/character)

              (= mode "devotions")
              (respec-character-devotions @globals/character)

              :else
              (throw (Throwable. (str "Unknown respec mode: " mode))))

            differences (clojure.data/diff @globals/character modified-character)]

        ;; Print how we're going to update the character
        (println "Updating the following fields:")
        (printer/print-map-difference differences)

        ;; Actually update the character
        (swap! globals/character (fn [oldval] modified-character))))))

(defn- log-level-get
  [settings]

  (:log-level settings))

(defn- log-level-set
  [settings log-level]

  (assoc settings :log-level log-level))

(defn- log-level-clear
  [settings]

  (dissoc settings :log-level))

(defn log-handler
  [[input token]]

  ;; If the user supplied no params, show the current log level
  (if (zero? (count token))
    (do
      (println "Current log level:")
      (u/print-indent 1)
      (println
       (yellow
        (u/keyword->str
         (or (log-level-get @globals/settings)
             :info)))))

    (let [log-level-str (str/lower-case (first token))]
      (cond
        ;; Did the user asked to stop logging?
        (= log-level-str "clear")
        (do
          (swap! globals/settings log-level-clear)
          (println (green "Ok!")))

        ;; The user asked to set a specific log level?
        (contains? t/-levels-set (keyword log-level-str))
        (let [log-level (keyword log-level-str)]
          (swap! globals/settings log-level-set (keyword log-level))
          (t/set-level! log-level)
          (println (green "Ok!")))

        :else
        (println (red "Unknown log level: " log-level-str))))))

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
#_(set-handler  [nil ["character-name" "Odie2"]])

#_(write-handler  [nil nil])
#_(run-query "recordname~gearweapons value~legendary levelreq <= 65")

#_(set-handler [nil ["equipment/0" "Mantle of the Weeping Eye" 100]])


#_(construct-item "Mantle of the Weeping Eye" @globals/db 100)

#_(load-db)

#_(skills-vec->skills-by-category (:skills @globals/character))
#_(respec-handler [nil ["all"]])

#_(set-handler  [nil ["character-name" "Odie2"]])
#_(write-handler  [nil nil])

#_(time (do
          (gd-edit.command-handlers/load-character-file
           (-> (dirs/get-save-dir-search-list)
               (first)
               (io/file "_Odie/player.gdc")
               (.getPath)
               ))
          nil))

#_(construct-item "Exalted Treads" @globals/db 100)

#_(set-item-handler  [nil ["inv/1/items" "legion warhammer of valor" "64"]])

#_(show-handler [nil ["inv/1/items"]])


(comment

  (time
   (set-handler  [nil ["inv/1/items/0" "stonehide iron maiden's mantle of the flesh hulk"]]))

  (time
   (set-handler  [nil ["inv/1/items/0" "vampiric legion warhammer of valor"]]))

  )
