(ns user
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

(defn load-character-file [f]
  "Loads the returns the character at the file location"
  (gdc/load-character-file f))

(defn load-character [f]
  "Load the character and set as global state, then returns the character"
  (gec/load-character-file f))

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
    (ge/repl-eval (ge/repl-read) ge/command-map)))

(comment
  (init)

  (load-character "")

  (do
    (gd-edit.command-handlers/load-character-file
     (-> (dirs/get-save-dir-search-list)
         (second)
         (io/file "_Odie/player.gdc")
         (.getPath)
         ))
    nil)

  (cmd "show character-name")

  (cmd "show skills")

  (cmd "respec")
)
