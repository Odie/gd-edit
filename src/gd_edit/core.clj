(ns gd-edit.core
  (:require [clojure.java.io :as io]
            [gd-edit.arz-reader :as arz-reader])
  (:import  [java.nio ByteBuffer]
            [java.nio.file Path Paths Files FileSystems StandardOpenOption]
            [java.nio.channels FileChannel])
  (:gen-class))



(defn -main
  [& args]
  (let [db (arz-reader/load-game-db "/Users/Odie/Dropbox/Public/GrimDawn/database/database.arz")]
    (println (count db) " records loaded")))

