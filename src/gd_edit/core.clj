(ns gd-edit.core
  (:require [clojure.java.io :as io]
            [gd-edit.arz-reader :as arz-reader]
            [gd-edit.arc-reader :as arc-reader]
            [gd-edit.utils :as utils]
            [gd-edit.globals :as globals]
            [gd-edit.command-handlers :as handlers]
            [gd-edit.jline :as jl]
            [gd-edit.game-dirs :as dirs]
            [clojure.string :as string]
            [jansi-clj.core :refer :all])
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
  (tokenize-input (jl/readline (green "> "))))


(defn split-at-space
  [str]
  (clojure.string/split str #"\s+"))

(def command-map
  {
   ["exit"] (fn [input] (System/exit 0))
   ["q"] (fn [input] (handlers/query-comand-handler input))
   ["qshow"] (fn [input] (handlers/query-show-handler input))
   ["qn"] (fn [input] (handlers/query-show-handler input))
   ["db"] (fn [input] (handlers/db-show-handler input))
   ["db" "show"] (fn [input] (handlers/db-show-handler input))
   ["show"] (fn [input] (handlers/show-handler input))
   ["show" "item"] (fn [input] (handlers/show-item-handler input))
   ["set"] (fn [input] (handlers/set-handler input))
   ["load"] (fn [input] (handlers/choose-character-handler input))
   ["write"] (fn [input] (handlers/write-handler input))
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

(defn- find-menu-command
  "Returns a menu item that matches the given target string, or nil"
  [target-str menu]

  (let [matched-menu-items (filter
                            (fn [[cmd-str]]
                              (= cmd-str target-str))

                            (:choice-map menu))]

    ;; If we can find a match, return the first match
    ;; This means if some menu items have the same command string,
    ;; only the first one will ever be picked
    (if-not (empty? matched-menu-items)
      (first matched-menu-items)
      nil)))

(defn- repl-print-menu
  "Print the given menu.

  A menu is a hashmap that has the following fields:

  :display-fn => called without parameters when available
  :choice-map => vector of [command-str display-string choice-function]

  Together, these fields are used to help the repl dynamically present
  information and alter the command map in a stateful manner. "
  [menu]

  (let [{:keys [display-fn choice-map]} menu]
    ;; Run the display function if available
    (if-not (nil? display-fn)
      (display-fn))

    ;; Display the choice-map
    (if-not (empty? choice-map)
      (do
        (doseq [[cmd-str disp-str] choice-map]
          (println (format "%s) %s" cmd-str disp-str)))
        (println)))))

(defn- repl-eval
  [[input tokens :as input-vec] command-map]

  ;; Try to find the "longest" command match
  ;; Basically, we're trying to find the most specific match.
  ;; This means we can put command handlers like "q" and "q show"
  ;; directly in the command map and figure out which one should be
  ;; called
  (let [menu-cmd (find-menu-command (first tokens) @globals/menu)
        menu-handler (if-not (nil? menu-cmd)
                       (nth menu-cmd 2)
                       nil)

        command (find-command tokens command-map)
        command-handler (command-map command)]

    (cond
      ;; if the entered command matches a menu item, run the handler function
      (not (nil? menu-handler))
      (menu-handler)

      ;; Otherwise, if the tokens can match something in the global command map,
      ;; run that.
      ;;
      ;; Remove the tokens that represent the command itself
      ;; They shouldn't be passed to the command handlers
      (not (nil? command-handler))
      (let [param-tokens (drop (count command) tokens)
            command-input-string (string/join " " param-tokens)]
        (command-handler [command-input-string param-tokens]))

      :else
      (println "Don't know how to handle this command"))))

(defn- repl-iter
  "Runs one repl iteration. Useful when the program is run from the repl"
  []

  (repl-print-menu @globals/menu)
  (repl-eval (repl-read) command-map)
  (println))

(defn- repl
  []

  (while true
    (repl-iter)))


(defn- initialize
  []

  (intern 'gd-edit.globals 'db (future (handlers/load-db)))
  (handlers/character-selection-screen!))


(defn -main
  [& args]

  (alter-var-root #'gd-edit.jline/use-jline (fn[oldval] true))
  (jansi-clj.core/install!)
  (initialize)
  (repl))

#_(initialize)
#_(time (do
          (reset! gd-edit.globals/character
                  (gd-edit.gdc-reader/load-character-file "/Users/Odie/Dropbox/Public/GrimDawn/main/_Hetzer/player.gdc"))
          nil))

;; Walk each db record
;; (def r
;;   (->> @gd-edit.globals/db
;;        (filter #(.startsWith (:recordname %1) "records/items/gearweapons"))
;;        (reduce (fn [set db-record]

;;                  (into set (keys db-record)))

;;                #{}
;;                )
;;        (filter #(not= :recordname %1))
;;        (filter #(.startsWith %1 "offensive"))
;;        (sort)
;;        ))

;; (def v
;;   (->> r
;;        (reduce (fn [set word]
;;                  (into set (clojure.string/split word #"(?=[A-Z])")))
;;                #{}
;;                )

;;        (filter #(> (count %1) 1))
;;        (map clojure.string/lower-case)
;;        (into #{})
;;        (sort)
;;        ))

;; (def s
;;   (->> r
;;        (filter #(-> (clojure.string/lower-case %1)
;;                     (.contains "leech")
;;                      )
;;        )))

;; (nth @gd-edit.globals/db 1)
