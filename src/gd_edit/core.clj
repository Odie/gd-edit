(ns gd-edit.core
  (:require [clojure.java.io :as io]
            [gd-edit.arz-reader :as arz-reader]
            [gd-edit.utils :as utils]
            )
  (:import  [java.nio ByteBuffer]
            [java.nio.file Path Paths Files FileSystems StandardOpenOption]
            [java.nio.channels FileChannel])
  (:gen-class))



(defn -main
  [& args]
  (let [[db-load-time db] (utils/time (arz-reader/load-game-db "/Users/Odie/Dropbox/Public/GrimDawn/database/database.arz"))]
    (println (count db) "records loaded in" (format "%.3f" (utils/nanotime->secs db-load-time)) "seconds")
  ))

