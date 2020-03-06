(ns gd-edit.commands.gamedir
  (:require [gd-edit.globals :as globals]
            [taoensso.timbre :as log]
            [gd-edit.utils :as u]
            [gd-edit.game-dirs :as dirs]
            [jansi-clj.core :refer [red green yellow]]
            [gd-edit.app-util :as au]))


(defn gamedir-clear-handler
  [[input tokens]]

  (au/setting-gamedir-clear!)
  (println "Ok!"))

(defn- gamedir-show
  []

  (println "Currently using this as game dir:")
  (println "    " (let [game-dir  (dirs/get-game-dir)]
                    (if (nil? game-dir)
                      (red "None")
                      game-dir))))

;; TODO Use better error handling here so we're not printing "Ok!" when there's an error
(defn- gamedir-set
  [game-dir]

  (if (empty? game-dir)
    (au/setting-gamedir-clear!)
    (au/setting-gamedir-set! game-dir))
  (println "Ok!"))

(defn gamedir-handler
  [[input tokens]]

  (log/debug "Entering gamedir-handler")
  (u/log-exp input)
  (u/log-exp tokens)

  (cond
    (= 0 (count tokens))
    (gamedir-show)

    :else
    (let [game-dir (first tokens)]
      (gamedir-set game-dir))))
