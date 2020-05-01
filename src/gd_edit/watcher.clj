(ns gd-edit.watcher
  (:require [gd-edit.globals :as globals]
            [gd-edit.io.stash :as stash]
            [gd-edit.game-dirs :as dirs]
            [hawk.core :as hawk]
            [jansi-clj.core :refer [yellow]]
            [gd-edit.utils :as u]))

(def transfer-stash-watcher (atom {}))

(defn loaded-stash-filepath
  []
  (:meta-stash-loaded-from @globals/transfer-stash))

(defn tf-watcher-started?
  []
  (not-empty @transfer-stash-watcher))

(defn tf-watcher-stop!
  []
  (when (tf-watcher-started?)
    (swap! transfer-stash-watcher #(hawk/stop! %))))


(defn attach-transfer-stash-to-character!
  []
  (swap! globals/character assoc :transfer-stashes (@globals/transfer-stash :stash)))

(defn load-transfer-stash!
  [stash-file]
  (reset! globals/transfer-stash (stash/load-stash-file stash-file))
  (when @globals/character
    (attach-transfer-stash-to-character!)))

(defn load-and-watch-transfer-stash!
  []

  (when-not (tf-watcher-started?)
    (when-let [stash-file (dirs/get-transfer-stash @globals/character)]
      ;; (println "stash-file" stash-file)
      (when (.exists stash-file)
        (println "Loading stash file:")
        (u/print-indent 1)
        (println (yellow (str stash-file)))
        (load-transfer-stash! stash-file))
      (when (.exists (.getParentFile stash-file))
        (reset! transfer-stash-watcher
                (hawk/watch! [{:paths [(.getParentFile stash-file)]
                               :handler (fn [ctx e]
                                          (when (and (#{:create :modify} (:kind e))
                                                     (= stash-file (:file e)))
                                            (load-transfer-stash! stash-file))
                                          ctx)}]))))))

(defn tf-watcher-restart!
  []
  (when (tf-watcher-started?)
    (tf-watcher-stop!))
  (load-and-watch-transfer-stash!))
