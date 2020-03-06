(ns gd-edit.commands.mod
  (:require [gd-edit.globals :as globals]
            [gd-edit.utils :as u]
            [gd-edit.stack :as stack]
            [gd-edit.app-util :as au]
            [gd-edit.game-dirs :as dirs]
            [clojure.java.io :as io]))

(defn- mod-selection-screen
  []

  (let [gamedir (dirs/get-game-dir)
        moddir (io/file gamedir "mods")
        mods (.listFiles moddir)]

    (if (or (not (.exists moddir))
            (empty? mods))
      (println "No mods installed")

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

                                 ;; Reload the database
                                 (au/load-db-in-background)

                                 (stack/pop! globals/menu-stack)
                                 )])))
                    []
                    (map-indexed vector mods))})))

(defn mod-handler
  [[input tokens]]

  (println "currently selected mod:")
  (u/print-indent 1)
  (if (:moddir @globals/settings)
    (println (u/last-path-component (:moddir @globals/settings)))
    (println "none")))

(defn mod-pick-handler
  [[input tokens]]

  (stack/push! globals/menu-stack (mod-selection-screen)))

(defn mod-clear-handler
  [[input tokens]]

  (swap! globals/settings dissoc :moddir)

  ;; Reload the database
  (au/load-db-in-background)

  (println "Ok!"))
