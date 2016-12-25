(ns gd-edit.core
  (:gen-class)
  (:require cheshire.core
            [clojure.java.io :as io]
            [clojure.string :as string]
            [gd-edit
             [arc-reader :as arc-reader]
             [arz-reader :as arz-reader]
             [command-handlers :as handlers]
             [game-dirs :as dirs]
             [globals :as globals]
             [jline :as jl]
             [utils :as utils]]
            [jansi-clj.core :refer :all])
  (:import java.nio.ByteBuffer
           java.nio.channels.FileChannel
           [java.nio.file Files FileSystems Path Paths StandardOpenOption]))

(defn- strip-quotes
  "Strip quotes from a string"
  [value]
  (clojure.string/replace value #"^\"|\"$" ""))

(defn- tokenize-input
  [input]

  [input
   (->> (into [] (re-seq #"\"[^\"]+\"|\S+" input))
        (map strip-quotes))])

(defn- repl-read
  []

  ;; Read a line
  (tokenize-input (jl/readline (green "> "))))


(defn split-at-space
  [str]
  (clojure.string/split str #"\s+"))

(def command-map
  {
   ["exit"]  (fn [input] (System/exit 0))
   ["q"]     (fn [input] (handlers/query-comand-handler input))
   ["qshow"] (fn [input] (handlers/query-show-handler input))
   ["qn"]    (fn [input] (handlers/query-show-handler input))
   ["db"]    (fn [input] (handlers/db-show-handler input))
   ["db" "show"] (fn [input] (handlers/db-show-handler input))
   ["show"]  (fn [input] (handlers/show-handler input))
   ["set"]   (fn [input] (handlers/set-handler input))
   ["load"]  (fn [input] (handlers/choose-character-handler input))
   ["write"] (fn [input] (handlers/write-handler input))
   ["class"] (fn [input] (handlers/class-handler input))
   ["class" "add"] (fn [input] (handlers/class-add-handler input))
   ["class" "remove"] (fn [input] (handlers/class-remove-handler input))
   ["help"] (fn [input] (handlers/help-handler input))
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
        _ (newline)
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
    (try
      (repl-iter)
      (catch Exception e
        (do
          (println "caught exception: " (.getMessage e))
          (clojure.stacktrace/print-stack-trace e)
          (newline))))))

(defn- initialize
  []

  (intern 'gd-edit.globals 'db (future (handlers/load-db)))
  (handlers/character-selection-screen!))

(defn- print-build-info
  []

  (if-let [info-file (io/resource "build.json")]
    (let [build-info (cheshire.core/parse-string (slurp info-file))]
      (println (bold (black (format "%s [build %s]" (build-info "app-name") (build-info "sha"))))))))

(defn -main
  [& args]

  (print-build-info)
  (println)
  (alter-var-root #'gd-edit.jline/use-jline (fn[oldval] true))
  (jansi-clj.core/install!)
  (initialize)
  (repl))

#_(initialize)
#_(time (do
          (reset! gd-edit.globals/character
                  (gd-edit.gdc-reader/load-character-file (.getPath (io/file (gd-edit.game-dirs/get-steam-cloud-save-dir) "_Odie/player.gdc"))))
          nil))
