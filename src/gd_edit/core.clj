(ns gd-edit.core
  (:require [clojure.java.io :as io]
            [gd-edit.arz-reader :as arz-reader]
            [gd-edit.arc-reader :as arc-reader]
            [gd-edit.utils :as utils]
            [gd-edit.globals]
            [gd-edit.command-handlers :as handlers]
            [gd-edit.jline :as jl]
            [clansi.core :refer [style]]
            [clojure.string :as string])
  (:import  [java.nio ByteBuffer]
            [java.nio.file Path Paths Files FileSystems StandardOpenOption]
            [java.nio.channels FileChannel])
  (:gen-class))


(defn- tokenize-input
  [input]

  [input
   (into [] (re-seq #"\"[^\"]+\"|\S+" input))])

(defn- repl-read
  []

  ;; Read a line
  (tokenize-input (jl/readline (style "> " :green ))))


(defn split-at-space
  [str]
  (clojure.string/split str #"\s+"))

(def command-map
  {
   ["exit"] (fn [input] (System/exit 0))
   ["q"] (fn [input] (handlers/query-comand-handler input))
   ["qshow"] (fn [input] (handlers/query-show-handler input))
   ["qn"] (fn [input] (handlers/query-show-handler input))
   ["q" "filter"] (fn [input] (handlers/query-filter-handler input))
   })

(defn- find-command
  "Try to find the \"longest\" command match"
  [tokens command-map]
  ;; We're trying to find the most specific match in the command map.
  ;; This means we can put command handlers like "q" and "q show"
  ;; directly in the command map and figure out which one should be
  ;; called
  (reduce
   (fn [accum item]
     ;; accum will be the longest match we found so far
     ;; Check if we can match against a command if we put one more token
     ;; into the command
     (let [command (conj accum item)]

       ;; Is the new command in the command-map?
       (if (command-map command)
         ;; If so, we've found a slightly longer match
         command

         ;; If not, we can't find a longer match and we're
         ;; done with the reduce
         (reduced accum)
         )))
   []
   tokens))


(defn- repl-eval
  [[input tokens :as input-vec] command-map]

  ;; Try to find the "longest" command match
  ;; Basically, we're trying to find the most specific match.
  ;; This means we can put command handlers like "q" and "q show"
  ;; directly in the command map and figure out which one should be
  ;; called
  (let [command (find-command tokens command-map)
        handler (command-map command)]

    (if (nil? handler)
      (println "Don't know how to handle this command")

      ;; Remove the tokens that represent the command itself
      ;; They shouldn't be passed to the command handlers
      (let [param-tokens (drop (count command) tokens)
            command-input-string (string/join " " param-tokens)]
        (handler [command-input-string param-tokens])))))

(defn- repl-iter
  "Runs one repl iteration. Useful when the program is run from the repl"
  []

  (repl-eval (repl-read) command-map)
  (println))


(defn- repl
  []

  (while true
    (repl-iter)))


(defn- get-db-filepath
  []

  (if (= (System/getProperty "os.name") "Mac OS X")
    "/Users/Odie/Dropbox/Public/GrimDawn/database/database.arz"
    "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Grim Dawn\\database\\database.arz"))

(defn- get-localization-filepath
  []

  (if (= (System/getProperty "os.name") "Mac OS X")
    "/Users/Odie/Dropbox/Public/GrimDawn/resources/text_en.arc"
    "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Grim Dawn\\resources\\text_en.arc"))

(defn- initialize
  []

  (let [[localization-load-time localization-table]
        (utils/timed
         (arc-reader/load-localization-table (get-localization-filepath)))

        [db-load-time db]
        (utils/timed
         (arz-reader/load-game-db (get-db-filepath)
                                  localization-table))]

    (reset! gd-edit.globals/db db)

    (println (count localization-table)
             "localization strings loaded in"
             (format "%.3f" (utils/nanotime->secs localization-load-time))
             "seconds")

    (println (count db)
             "records loaded in"
             (format "%.3f" (utils/nanotime->secs db-load-time))
             "seconds")

    (println)
    (println "Ready to rock!")
    (println)
  ))


(defn -main
  [& args]

  (alter-var-root #'gd-edit.jline/use-jline (fn[oldval] true))
  (initialize)
  (repl))

#_(initialize)
