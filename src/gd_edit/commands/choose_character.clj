(ns gd-edit.commands.choose-character
  (:require [jansi-clj.core :refer [red green yellow]]
            [gd-edit.globals :as globals]
            [gd-edit.app-util :as au]
            [gd-edit.commands.write :as commands.write]
            [clojure.java.io :as io]
            [gd-edit.game-dirs :as dirs]
            [clojure.string :as str]
            [gd-edit.utils :as u]
            [gd-edit.stack :as stack]
            [gd-edit.io.stash :as stash]
            [gd-edit.printer :as printer]))

(defn- character-manipulation-screen
  []
  {:display-fn
   (fn []
     (println "Character: " (:character-name @globals/character)))

   :choice-map [["r" "reload" (fn [] (au/load-character-file (@globals/character :meta-character-loaded-from)))]
                ["w" "write" (fn[] (commands.write/write-handler [nil]))]]})


(declare character-manipulation-screen!)

(defn- load-character-file
  [savepath]
  (println "Loading from:")
  (u/print-indent 1)
  (println (yellow savepath))
  (au/load-character-file savepath)
  (character-manipulation-screen!))

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
                                     (let [char-name (u/last-path-component (.getPath dir))]
                                       (if (= \_ (first char-name))
                                         (subs char-name 1)
                                         char-name))
                                     (dirs/save-dir-type dir))         ; menu display string
                             (fn []                               ; function to run when selected
                               (let [savepath (.getPath (io/file dir "player.gdc"))]
                                 (load-character-file savepath)))])))
                  []
                  (map-indexed vector save-dirs))}))

(defn character-selection-screen! [] (stack/replace-last! gd-edit.globals/menu-stack (character-selection-screen)))
(defn character-manipulation-screen! [] (stack/replace-last! gd-edit.globals/menu-stack (character-manipulation-screen)))

(defn choose-character-handler
  [[input [param]]]

  ;; Did the user provide a parameter?
  (if param

    (let [result
          (or
           ;; The param might be a filepath to the save file
           (let [character-filepath param
                 character-file (io/file character-filepath)]
             (when (and (.exists character-file)
                        (.isFile character-file))
               (load-character-file character-file)))

           ;; The param might be a character name
           (let [character-name param
                 characters (au/locate-character-files param)]
             (cond
               (zero? (count characters))
               (println (red "Sorry,") (format "cannot find a character named \"%s\"" character-name))

               (> (count characters) 1)
               (do
                 (println (red "Sorry,") "there is more than one character with that name at:")
                 (doseq [f characters]
                   (u/print-indent 1)
                   (println (yellow f))))

               :else
               (load-character-file (first characters)))))])

    ;; The user did not provide a name or a path
    ;; Show a menu to let the user choose from
    (do
      (reset! globals/character (empty @globals/character))
      (character-selection-screen!))))

(defn choose-or-manipulate-character-screen!
  []

  (if (empty? @globals/character)
    (character-selection-screen!)
    (character-manipulation-screen!)))
