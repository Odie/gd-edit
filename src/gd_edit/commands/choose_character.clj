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
            [gd-edit.io.stash :as stash]))

(defn- character-manipulation-screen
  []
  {:display-fn
   (fn []
     (println "Character: " (:character-name @globals/character)))

   :choice-map [["r" "reload" (fn [] (au/load-character-file (@globals/character :meta-character-loaded-from)))]
                ["w" "write" (fn[] (commands.write/write-handler [nil]))]]})

(defn- is-cloud-save?
  [dir]
  (not (nil? (some #(str/starts-with? dir %)
                   (map #(.getParent (io/file %)) (dirs/get-steam-cloud-save-dirs))))))

(defn- is-mod-save?
  [dir]

  (let [components (u/path-components (str dir))
        length (count components)]
    (if (and (u/case-insensitive= (components (- length 3)) "save")
             (u/case-insensitive= (components (- length 2)) "user"))
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

(declare character-manipulation-screen!)

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
                                     (save-dir-type dir))         ; menu display string
                             (fn []                               ; function to run when selected
                               (let [savepath (.getPath (io/file dir "player.gdc"))]
                                 (println "Loading from:")
                                 (u/print-indent 1)
                                 (println (yellow savepath))
                                 (au/load-character-file savepath)
                                 (character-manipulation-screen!))
                               )])))
                  []
                  (map-indexed vector save-dirs))}))

(defn character-selection-screen! [] (stack/replace-last! gd-edit.globals/menu-stack (character-selection-screen)))
(defn character-manipulation-screen! [] (stack/replace-last! gd-edit.globals/menu-stack (character-manipulation-screen)))

(defn choose-character-handler
  [[input tokens]]

  (reset! globals/character (empty @globals/character))
  (character-selection-screen!))
