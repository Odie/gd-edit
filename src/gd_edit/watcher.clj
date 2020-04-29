(ns gd-edit.watcher
  (:require [gd-edit.globals :as globals]
            [gd-edit.io.stash :as stash]
            [gd-edit.game-dirs :as dirs]
            [hawk.core :as hawk]))

(def transfer-stash-watcher (atom {}))

(defn tf-watcher-started?
  []
  (not-empty @transfer-stash-watcher))

(defn tf-watcher-stop!
  []
  (swap! transfer-stash-watcher #(hawk/stop! %)))

(defn load-transfer-stash!
  []
  (when-let [stash-file (dirs/get-transfer-stash)]
    (reset! globals/transfer-stash (stash/load-stash-file stash-file))
    (when @globals/character
      (swap! globals/character assoc :transfer-stashes (@globals/transfer-stash :stash)))))

(defn load-and-watch-transfer-stash!
  []

  (when-not (tf-watcher-started?)
    (when-let [stash-file (dirs/get-transfer-stash)]
      (load-transfer-stash!)
      (reset! transfer-stash-watcher
              (hawk/watch! [{:paths [(.getParentFile stash-file)]
                             :handler (fn [ctx e]
                                        (when (and (#{:create :modify} (:kind e))
                                                   (= stash-file (:file e)))

                                          (println "reloading transfer stash!")
                                          (load-transfer-stash!))
                                        ctx)}])))))
