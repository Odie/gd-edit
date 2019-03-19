(ns repl
  (:require [gd-edit
             [core :as ge]
             [command-handlers :as gec]
             [globals :as globals]
             [gdc-reader :as gdc]
             [printer]
             [game-dirs :as dirs]]
            [clojure.java.io :as io])
  (:import [java.io BufferedReader StringReader]))

(defn init []
  "Initialize various globals, such as db and db-index"
  (gd-edit.core/initialize))

(defn resolve-save-file [f]
  ;; If the given file exists, nothing needs to be done to resolve the path
  (if (.isFile (io/file f))
    f

    (->> (dirs/get-save-dir-search-list)  ;; look in all save dirs
         (mapcat #(vector (io/file % f)   ;; just `f`, full path to save file
                          (io/file % (str "_" f) "player.gdc"))) ;; _`f`/player.gdc, `f` is just the character dir name
         (filter #(.isFile %))
         first)))

(defn load-character-file [f]
  "Loads the returns the character at the file location"
  (gdc/load-character-file f))

(defn load-character [f]
  "Load the character and set as global state, then returns the character"

  (if-let [target-file (resolve-save-file f)]
    (gec/load-character-file target-file)
    (println "File not found: " f)))

(defn write-character-file [character f]
  (gdc/write-character-file character f))

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

(defn cmd [s]
  "Send the command through the repl pipeline as if it were entered in the console"
  (binding [*in* (io/reader (StringReader. s))]
    (ge/repl-eval (ge/repl-read) ge/command-map)
    nil))

(comment
  (init)

  (load-character "Odie")

  (write-character-file @globals/character "/tmp/player.gdc")

  (cmd "show character-name")

  (cmd "show skills")

  (cmd "respec")
)
