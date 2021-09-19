(ns gd-edit.commands.update
  (:require [gd-edit.self-update :as su]
            [gd-edit.utils :as u]))

(defn update-handler
  [[input tokens]]

  (let [result (su/try-self-update)]
    (when (= result :up-to-date)
      (u/print-line "Already running latest version"))))
