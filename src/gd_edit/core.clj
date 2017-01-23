(ns gd-edit.core
  (:gen-class)
  (:require [clojure.edn :as edn]
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
            [jansi-clj.core :refer :all]
            [gd-edit.utils :as u]
            [clojure.core.async :as async :refer [thread]]
            [progress.file :as progress])
  (:import java.nio.ByteBuffer
           java.nio.channels.FileChannel
           java.lang.ProcessBuilder
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
   ["gamedir"] (fn [input] (handlers/gamedir-handler input))
   ["gamedir" "clear"] (fn [input] (handlers/gamedir-clear-handler input))
   ["savedir"] (fn [input] (handlers/savedir-handler input))
   ["savedir" "clear"] (fn [input] (handlers/savedir-clear-handler input))
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

(declare startup-sanity-checks)

(defn- initialize
  []

  ;; Try to load the settings file if it exists
  (handlers/load-settings-file)

  ;; Run sanity checks and print any error messages
  (startup-sanity-checks)

  ;; Try to load the game db
  (handlers/load-db-in-background)

  ;; Setup the first screen in the program
  (handlers/character-selection-screen!))

(defn- print-build-info
  []

  (if-let [info-file (io/resource "build.edn")]
    (let [build-info (edn/read-string (slurp info-file))]
      (println (bold (black (format "%s [build %s]" (build-info :app-name) (build-info :sha))))))))

(defn- check-save-dir-found?!
  [verbose]

  (let [save-file-dirs (dirs/get-all-save-file-dirs)]
    (if (empty? save-file-dirs)
      (do
        (println (red "No save files can be located"))
        (println "The following locations were checked:")
        (doseq [loc (dirs/get-save-dir-search-list)]
          (println (str "    " loc)))
        false ;; return false to indicate that we failed the test
        )

      (do
        (when verbose
          (let [actual-dirs (reduce (fn [result item]
                                      (conj result (.getParent (io/file item))))
                                    #{}
                                    save-file-dirs)]
            (println "save directories:")
            ;;(println actual-dirs)
            (doseq [loc actual-dirs]
              (println (str "    " loc)))
          ))
        true))))

(defn- check-game-dir-found?!
  [verbose]

  (if-not (dirs/get-game-dir)
    (do
      (println (red "Game directory cannot be located"))
      (println "The following locations were checked:")
      (doseq [dir (dirs/get-game-dir-search-list)]
              (println (str "    " dir)))
      (newline)
      (println "Some editor functions such as db queries and changing items and equipment won't work properly.")
      false)

    (do
      (when verbose
        (let [actual-dirs (reduce (fn [result item]
                                    (conj result (.getParent (io/file item))))
                                  #{}
                                  [(-> (dirs/get-db-filepath)
                                       (.getParent))])]
          (println "game directory:")
          (doseq [loc actual-dirs]
            (println (str "    " loc)))
          ))
      true)))

(defn- startup-sanity-checks
  []

  (let [passes-required-checks (check-save-dir-found?! true)]
    (newline)
    (check-game-dir-found?! true)
    (newline)
    passes-required-checks))

(defn- get-build-info
  "Get the build info associated with the current build"
  []
  (when-let [info-file (io/resource "build.edn")]
    (edn/read-string (slurp info-file)))
  {:timestamp (java.util.Date.)})

(defn- is-newer?
  "Check if build-a is newer than build-b"
  [build-a build-b]

  (.isAfter (.toInstant (:timestamp build-a))
            (.toInstant (:timestamp build-b))))

(defn- fetch-latest-build-info
  "Get the information about the latest available build from network
  WARNING: This may block for a long time."
  []
  (let [url (java.net.URL. "https://dl.dropboxusercontent.com/u/3848680/GrimDawn/editor/latest-build.edn")
        conn (.openConnection url)]

    ;; Setup reasonable timeouts to contact the server with
    (doto conn
      (.setConnectTimeout 10000)
      ;; (.setReadTimeout 1000)
      )

    ;; Return the contents of the file to the caller
    (slurp (.getInputStream conn))))

(defn- has-new-version
  "Check if the known latest build is newer than the current running build

  WARNING: Long running function
  We're going over the network to fetch info regarding the latest build.
  It may take a second or it may never return because network isn't available."
  []

  (when-let [current-build-info (get-build-info)]
    (when-let [latest-build-info (fetch-latest-build-info)]

      (is-newer? latest-build-info current-build-info))))

(defn- fetch-latest-build
  [build-info]
  (let [url (java.net.URL. "https://dl.dropboxusercontent.com/u/3848680/GrimDawn/editor/latest-SNAPSHOT-standalone.exe")
        conn (.openConnection url)
        file (java.io.File/createTempFile "gd-edit" ".exe")]

    ;; Setup reasonable timeouts to contact the server with
    (doto conn
      (.setConnectTimeout 10000)
      ;; (.setReadTimeout 1000)
      )

    ;; Try to copy the file contents to a temp location
    (println "Downloading new version")

    (progress/with-file-progress file :filesize (:filesize build-info)
      (io/copy (.getInputStream conn) file))

    ;; Return the temp location to caller
    file))

(defn- fetch-latest-version
  "Attempts to fetch the latest version of the program.
  Returns either:
    :up-to-date - if already running the latest version
    tempfile - if can be

  Can throw exception. We're performing various networking tasks. Any number of conditions may arise.
  It's not clear what exactly might be thrown here.
  It's up to the caller to handle network error conditions."
  []

  (when-let [latest-build-info (fetch-latest-build-info)]
    (let [current-build-info (get-build-info)]

      ;; Is the latest build newer than the one we're running?
      (if-not true;(is-newer? latest-build-info (get-build-info))

        ;; If not, then say we don't need to do anything
        :up-to-date

        ;; Otherwise, try to fetch the latest version
        (fetch-latest-build latest-build-info)))))

(defmacro fmt [^String string]
  (let [-re #"#\{(.*?)\}"
        fstr (clojure.string/replace string -re "%s")
        fargs (map #(read-string (second %)) (re-seq -re string))]
    `(format ~fstr ~@fargs)
    ))

(defn- restart-self
  [new-exe]

  (println "Restarting...")
  (let [running-exe-path (io/file (System/getProperty "java.class.path"))
        backup-path (io/file (str running-exe-path ".bak"))
        restart-script-path (io/file (utils/working-directory) "restart.bat")]

    ;; Write out the restart script
    (spit restart-script-path
     (fmt
      "
        @echo off
        echo Preparing to restart gd-edit...

        REM remove old backup file
        del /F #{backup-path}
        if %%ERRORLEVEL%% neq 0 goto backup_error

        REM rename the original exe as a backup
        move #{running-exe-path} #{backup-path}
        if %%ERRORLEVEL%% neq 0 goto backup_error

        REM move the new exe to where the original exe was
        move #{(str new-exe)} #{running-exe-path}
        if %%ERRORLEVEL%% neq 0 goto replace_error

        REM finally, start the new version
        echo Restarting gd-edit...
        start #{running-exe-path}
        exit 0

        backup_error:
        echo Could not create backup file. Aborting self update.
        pause
        exit 1

        replace_error:
        echo Could not move new version to correct location.
        echo Attempting to restore backup.
        echo If this doesn't work, please manually rename
        echo #{backup-path} => #{running-exe-path}
        move #{backup-path} #{running-exe-path}
        start #{running-exe-path}
        pause
        exit 1
      "
      ))
    (.setExecutable restart-script-path true)

    ;; Start the restart script
    (-> (ProcessBuilder. ["cmd.exe" "/C" "start" (str restart-script-path)])
        (.start))

    (System/exit 0)))

(defn- try-self-update
  []
  (let [fetch-result (fetch-latest-version)]
    (cond
      (instance? java.io.File fetch-result)
      (restart-self fetch-result)

      :else
      fetch-result)))


(defn -main
  [& args]

  ;; Enable cross-platform ansi color handling
  (alter-var-root #'gd-edit.jline/use-jline (fn[oldval] true))
  (jansi-clj.core/install!)

  ;; (println (clojure-version))
  (print-build-info)
  (println)

  (try-self-update)

  (do
    (initialize)
    (repl)))

#_(initialize)
#_(time (do
          (reset! gd-edit.globals/character
                  (gd-edit.gdc-reader/load-character-file
                   (-> (dirs/get-save-dir-search-list)
                        (first)
                        (io/file "_Odie/player.gdc")
                        (.getPath)
                        )))
          nil))
