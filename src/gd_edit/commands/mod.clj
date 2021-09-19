(ns gd-edit.commands.mod
  (:require [gd-edit.globals :as globals]
            [gd-edit.utils :as u]
            [gd-edit.stack :as stack]
            [gd-edit.app-util :as au]
            [gd-edit.game-dirs :as dirs]
            [clojure.java.io :as io]
            [gd-edit.watcher :as watcher]))

(defn- on-mod-change
  []

  (au/load-db-in-background)
  (watcher/tf-watcher-restart!))

(defn- mod-selection-screen
  []

  (let [gamedir (dirs/get-game-dir)
        moddir (io/file gamedir "mods")
        mods (.listFiles moddir)]

    (if (or (not (.exists moddir))
            (empty? mods))
      (u/print-line "No mods installed")

      {:display-fn nil
       :choice-map (reduce
                    (fn [accum [idx moddir]]
                      (let [display-idx (inc idx)]
                        (conj accum
                              [(str display-idx)
                               (str (u/last-path-component (str moddir)))
                               (fn []
                                 ;; Update the global state and save the settings file
                                 (swap! globals/settings assoc :moddir (str moddir))

                                 (on-mod-change)

                                 (stack/pop! globals/menu-stack)
                                 )])))
                    []
                    (map-indexed vector mods))})))

(defn mod-handler
  [[input tokens]]

  (u/print-line "currently selected mod:")
  (u/print-indent 1)
  (if (:moddir @globals/settings)
    (u/print-line (u/last-path-component (:moddir @globals/settings)))
    (u/print-line "none")))

(defn mod-pick-handler
  [[input tokens]]

  (stack/push! globals/menu-stack (mod-selection-screen)))

(defn mod-clear-handler
  [[input tokens]]

  (swap! globals/settings dissoc :moddir)

  (on-mod-change)

  (u/print-line "Ok!"))
