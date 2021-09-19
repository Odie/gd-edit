(ns gd-edit.commands.savedir
  (:require [clojure.string :as str]
            [gd-edit.game-dirs :as dirs]
            [gd-edit.app-util :as au]
            [gd-edit.commands.choose-character :as commands.choose-character]
            [gd-edit.utils :as u]))

(defn- maybe-return-to-character-selection-screen
  []

  ;; If no character has been loaded yet, reload the screen...
  (if (not (au/character-loaded?))
    (commands.choose-character/choose-character-handler nil)

    (u/print-line (str/join "\n"
                       ["Savedir changed."
                        "You can use the \"load\" command to return to the character selection screen."
                        "Any unsaved changes for the current character will be lost."]))))

(defn savedir-clear-handler
  [[input tokens]]

  (au/setting-savedir-clear!)
  (u/print-line "Ok!")
  (maybe-return-to-character-selection-screen))

(defn savedir-handler
  [[input tokens]]

  (cond
    (= 0 (count tokens))
    (do
      (u/print-line "Currently looking through these directories for save files:")
      (doseq [loc (dirs/get-save-dir-search-list)]
        (u/print-line (str "    " loc))))

    :else
    (let [save-dir (first tokens)]

      (if (empty? save-dir)
        (au/setting-savedir-clear!)
        (au/setting-savedir-set! save-dir))
      (u/print-line "Ok!")
      (maybe-return-to-character-selection-screen))))
