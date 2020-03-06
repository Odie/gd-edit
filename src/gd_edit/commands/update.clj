(ns gd-edit.commands.update
  (:require [gd-edit.self-update :as su]))

(defn update-handler
  [[input tokens]]

  (let [result (su/try-self-update)]
    (when (= result :up-to-date)
      (println "Already running latest version"))))
