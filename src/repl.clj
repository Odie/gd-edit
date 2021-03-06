(ns repl
  (:require [clojure.java.io :as io]
            [gd-edit.core :as ge]
            [gd-edit.game-dirs :as dirs]
            [gd-edit.globals :as globals]
            [gd-edit.io.gdc :as gdc]
            [gd-edit.printer]
            [gd-edit.utils :as u]
            [gd-edit.app-util :as au]
            [gd-edit.db-utils :as dbu]
            [clojure.string :as str]
            [gd-edit.structure-walk :as sw]
            [gd-edit.equation-eval :as eq]
            [gd-edit.printer :as printer])
  (:import [java.io StringReader]))

(defn class-compile [ns-name]

  (let [tmp-dir *compile-path*
        sanitized-ns-name (-> (str ns-name)
                              (str/replace  "-" "_")
                              (str/replace  "." "/"))
        src-file (io/file "src" (str sanitized-ns-name ".clj"))
        dst-file (io/file (str tmp-dir  "/" (clojure.string/join "/" (clojure.string/split sanitized-ns-name #"\.")) ".clj"))]

    ;; Copy the file into the compile path
    (clojure.java.io/make-parents dst-file)
    (io/copy src-file dst-file)

    (binding [*compile-path* (str tmp-dir)] ;; str here is important, the compile doesn't like something else
      (try
        (compile (symbol ns-name))
        (catch java.io.FileNotFoundException e
          ;; *compile-path* also needs to be on the classpath! (how can we check this?!
          (throw (ex-info (str "*compile-path* : " (pr-str (str tmp-dir)) " isn't on the classpath")
                          {:compile-path *compile-path*
                           :clj-file dst-file}
                          #_e ;; If we add the cause, the above error doesn't show directly
                          )))))

    {:clj-file dst-file
     :ns-name ns-name}))

(defn init
  "Initialize various globals, such as db and db-index"
  []
  (ge/initialize))

(defn resolve-save-file [f]
  ;; If the given file exists, nothing needs to be done to resolve the path
  (if (.isFile (io/file f))
    f

    (first (au/locate-character-files f))))

(defn load-character-file
  "Loads the returns the character at the file location"
  [f]
  (gdc/load-character-file f))

(defn load-character
  "Load the character and set as global state, then returns the character"
  [f]

  (if-let [target-file (resolve-save-file f)]
    (au/load-character-file target-file)
    (println "File not found: " f)))

(defn write-character-file [character f]
  (gdc/write-character-file character f))

(defn print-character [character]
  (gd-edit.printer/print-map character))

(defn character []
  @globals/character)

(defn db []
  @globals/db)

(defn db-index []
  @globals/db-index)

(defn cmd
  "Send the command through the repl pipeline as if it were entered in the console"
  [s]
  (binding [*in* (io/reader (StringReader. s))]
    (ge/repl-eval (ge/repl-read) ge/command-map)
    nil))


(defn collect-record-names
  [coll [k value]]
  (cond
    (= k :recordname)
    coll

    (and
     (string? value)
     (.startsWith value "records/"))
    (conj coll value)

    (or
     (list? value)
     (vector? value))
    (concat
     coll
     (filter #(and (string? %)
                   (.startsWith % "records/")) value))

    :else
    coll))

(defn related-db-records
  [record db-and-index]

  (let [;; Collect all values in the record that look like a db record
        related-recordnames (->> record
                                 (reduce collect-record-names #{}))

        ;; Retrieve all related records by name
        related-records (map #((:index db-and-index) %1) related-recordnames)]

    related-records))

(defn referenced-records
  [record]
  (related-db-records (if (string? record)
                        ((db-index) record)
                        record)
                      @globals/db-and-index))

(defn record-might-be-item
  [record]
  (some (fn [[k _]]
          (u/ci-match k "item")) record))

(defn reachable-records
  [roots]

  (let [interesting-classes #{"LevelTable" "LootItemTable_DynWeight" "LootRandomizer"}]
    (loop [front roots
           explored []]
      (if (empty? front)
        explored

        (let [node (first front)
              ;; _ (println "node: " (:recordname node))
              related (->> (related-db-records node @globals/db-and-index)
                           (filter #(or (= "LevelTable" (get % "Class"))
                                        (u/ci-match (get % "Class") "loot")
                                        ;; (= "LootItemTable_DynWeight" (get %"Class"))
                                        (record-might-be-item %)
                                        ))
                           )]
          (recur
           (concat
            (rest front)
            related)
           (conj explored node)))))))

(defn get-at-path
  [character path]
  (->> (str/split path #"/")
       (sw/walk character )
       :found-item))

(comment
  (init)

  (load-character "Odie")

  (cmd "delete OdieTest")

  (write-character-file @globals/character "/tmp/player.gdc")

  (cmd "show character-name")

  (cmd "show skills")

  (cmd "respec")

  (cmd "mod pick")

  (cmd "mod")

  (u/timed-readable
   (cmd "set inv/1/items \"Hydra bow\""))


  (let [loot-masters (->> (db)
                          (filter #(= "LootMasterTable" (get % "Class"))))]

    (->> (reachable-records loot-masters)
         (map #(get % "Class"))
         (into #{})
         ;; count
         )
    )

  (->> (reachable-records [((db-index) "records/items/loottables/mastertables/mt_crafting_blueprints_runes_c201.dbr")])
       ;; (map #(get % "Class"))
       ;; (into #{})
       count
       )

  (referenced-records
   "records/items/loottables/mastertables/mt_crafting_blueprints_runes_c201.dbr")

  (referenced-records
   "records/items/loottables/blueprints/lt_blueprints_mobilityrunes_c201.dbr")

  (referenced-records
  "records/items/loottables/materia/tdyn_comp_magical_a01.dbr")

  (referenced-records
   "records/items/autopickup/materia/compa_polishedemerald.dbr")

  (referenced-records
   "records/items/autopickup/materia/compa_markofillusions.dbr")

  (cmd "set inv/1/items \"Glyph of Seismic Strength\"")

  (cmd "db")

  (cmd "db records/items/autopickup/materia/")

  (require '[gd-edit.printer :as printer])

  (printer/show-item (get-at-path @globals/character "equipment/0"))

)
