(ns gd-edit.commands.delete
  (:require [clojure.java.io :as io]
            [gd-edit.utils :as u]
            [jansi-clj.core :refer [red yellow]]
            [gd-edit.app-util :as au]
            [gd-edit.globals :as globals]
            [gd-edit.stack :as stack]
            [gd-edit.game-dirs :as dirs]
            [gd-edit.commands.choose-character :as commands.choose-character])
  (:import [com.sun.jna.platform FileUtils]))

(defn- delete-character-file
  [savepath]

  (let [target (io/file savepath)
        target (cond-> target
                 (.isFile target)
                 (.getParentFile))]
    (u/print-line "Moving character to trash:")
    (u/print-indent 1)
    (u/print-line (yellow target))

    (try
      (.moveToTrash (FileUtils/getInstance)
                    (into-array [target]))
      (catch Exception _ (u/print-line (red "Error!") "Could not move files to trash!")))

    ;; If the loaded character was deleted, unload the character from memory
    (when (= (:meta-character-loaded-from @globals/character) (io/file savepath))
      (reset! globals/character {}))

    (commands.choose-character/choose-or-manipulate-character-screen!)))

(defn- character-selection-screen
  []

  ;; grab a list save directories where a "player.gdc" file exists
  (let [save-dirs (dirs/get-all-save-file-dirs)]

    {:display-fn
     (fn []
       (if (empty? save-dirs)
         (u/print-line (red "No save files found"))
         (u/print-line "Please choose a character to delete:")))

     ;; generate the menu choices
     ;; reduce over save-dirs with each item being [index save-dir-item]
     :choice-map (doall (for [[idx dir] (u/indexed save-dirs)]
                          (let [display-idx (inc idx)]
                            [(str display-idx)                    ; command string
                             (format "%s (%s save)"
                                     (let [char-name (u/last-path-component (.getPath dir))]
                                       (if (= \_ (first char-name))
                                         (subs char-name 1)
                                         char-name))
                                     (dirs/save-dir-type dir))         ; menu display string
                             (fn []                               ; function to run when selected
                               (let [savepath (.getPath (io/file dir "player.gdc"))]
                                 (delete-character-file savepath)))])))}))

(defn character-selection-screen! [] (stack/replace-last! globals/menu-stack (character-selection-screen)))

(defn delete-handler
  [[_ [param]]]

  ;; Did the user provide a parameter?
  (if param

    (or
     ;; The param might be a filepath to the save file
     (let [character-filepath param
           character-file (io/file character-filepath)]
       (when (and (.exists character-file)
                  (.isFile character-file))
         (delete-character-file character-file)
         :ok))

     ;; The param might be a character name
     (let [character-name param
           characters (au/locate-character-files param)]
       (cond
         (zero? (count characters))
         (u/print-line (red "Sorry,") (format "cannot find a character named \"%s\"" character-name))

         (> (count characters) 1)
         (do
           (u/print-line (red "Sorry,") "there is more than one character with that name at:")
           (doseq [f characters]
             (u/print-indent 1)
             (u/print-line (yellow f))))

         :else
         (delete-character-file (first characters)))))

    ;; The user did not provide a name or a path
    ;; Show a menu to let the user choose from
    (character-selection-screen!)))
