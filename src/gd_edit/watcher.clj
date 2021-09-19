(ns gd-edit.watcher
  (:require [gd-edit.globals :as globals]
            [gd-edit.io.stash :as stash]
            [gd-edit.game-dirs :as dirs]
            [jansi-clj.core :refer [yellow]]
            [gd-edit.utils :as u]
            [clojure.java.io :as io]
            [clojure.data :as data]))

;;------------------------------------------------------------------------------
;; Generic file watching facility
;;
;;  The Java file watcher service seem to have these two limitations in windows:
;;  1. It cannot watch a single file
;;  2. It introduces some kind of lock on the directory
;;
;;  On top of this, the library `hawk` that was being used to register for file
;;  watching seem add watches to the entire directory tree.
;;
;;  The final result is that directories inside the subtree of the file being
;;  watched cannot be moved or deleted.
;;
;;  This all seem a little silly. While it's possible to introduce a modified
;;  copy of `hawk` and see if this can be fixed, it's probably easier to start
;;  a thread that polls for file changes. We're only interested in watching
;;  a single file anyway.
;;
;;------------------------------------------------------------------------------
(def watcher-thread (atom {}))

(defn watcher-started?
  []
  (when-let [thread (:thread @watcher-thread)]
    (when (.isAlive thread)
      true)))

(defn watcher-stop!
  []
  (when (watcher-started?)
    ;; Set a flag to stop the watcher thread
    (swap! watcher-thread assoc :stop true)

    ;; Wait for the thread to exit
    (.join (:thread @watcher-thread))

    ;; Cleanup outdated data
    (swap! watcher-thread dissoc :stop :thread)))

(defn file-infos-for-paths
  [paths]
  (->> paths
       (map (fn [path]
              (let [f (io/file path)]
                [(str path) (cond
                              (not (.exists f))
                              :does-not-exist

                              :else
                              (.lastModified f))])))
       (into {})))

(defn start-watch-thread
  [callback shared-vars]

  (let [get-file-infos (fn [] (file-infos-for-paths (:paths @shared-vars)))

        _ (swap! shared-vars assoc :last-file-infos (get-file-infos))

        thread (Thread. (fn []
                          (while (not (:stop @shared-vars))
                            (let [new-infos (get-file-infos)
                                  last-file-infos (:last-file-infos @shared-vars)
                                  delta (data/diff new-infos last-file-infos)]

                              (doseq [changed-file (into #{} (concat (keys (first delta)) (keys (second delta))))]
                                (callback changed-file))

                              (swap! shared-vars assoc :last-file-infos new-infos)

                              (Thread/sleep 250)))))]
    (.start thread)
    thread))

(defn watcher-start-thread!
  [callback paths]

  ;; Don't start the watch thread again
  (when-not (watcher-started?)
    (reset! watcher-thread {:paths paths})
    (let [thread (start-watch-thread callback watcher-thread)]
      (swap! watcher-thread assoc :thread thread))))

(defn watcher-change-paths!
  [paths]

  (when (watcher-started?)
    (let [new-infos (file-infos-for-paths paths)]
      (swap! watcher-thread #(assoc % :paths paths :last-file-infos new-infos)))))



;;------------------------------------------------------------------------------
;; Transfer stash watching
;;------------------------------------------------------------------------------
(defn loaded-stash-filepath
  []
  (:meta-stash-loaded-from @globals/transfer-stash))

(defn tf-watcher-started?
  []
  (watcher-started?))

(defn tf-watcher-stop!
  []
  (when (tf-watcher-started?)
    (watcher-stop!)))

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

  (when (and
         (not (empty? @globals/character))
         (not (tf-watcher-started?)))
    (when-let [stash-file (dirs/get-transfer-stash @globals/character)]
      ;; (u/print-line "stash-file" stash-file)
      (when (.exists stash-file)
        (u/print-line "Loading stash file:")
        (u/print-indent 1)
        (u/print-line (yellow (str stash-file)))
        (load-transfer-stash! stash-file)

        (watcher-start-thread! (fn [path]
                                 (when (= stash-file (io/file path))
                                   ;; (u/print-line "Reloading stash file!")
                                   (u/wait-file-stopped-growing stash-file)
                                   (load-transfer-stash! stash-file)))
                               [stash-file])))))

(defn tf-watcher-restart!
  []
  (when (tf-watcher-started?)
    (tf-watcher-stop!))
  (load-and-watch-transfer-stash!))
