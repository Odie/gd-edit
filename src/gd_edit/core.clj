(ns gd-edit.core
  (:require [clojure.java.io :as io]
            [gd-edit.arz-reader :as arz-reader]
            [gd-edit.arc-reader :as arc-reader]
            [gd-edit.utils :as utils]
            )
  (:import  [java.nio ByteBuffer]
            [java.nio.file Path Paths Files FileSystems StandardOpenOption]
            [java.nio.channels FileChannel])
  (:gen-class))



(defn -main
  [& args]
  (let [[localization-load-time localization-table]
        (utils/timed
         (arc-reader/load-localization-table "/Users/Odie/Dropbox/Public/GrimDawn/resources/text_en.arc"))

        [db-load-time db]
        (utils/timed
         (arz-reader/load-game-db "/Users/Odie/Dropbox/Public/GrimDawn/database/database.arz"
                                  localization-table))]

    (println (count localization-table)
             "localization strings loaded in"
             (format "%.3f" (utils/nanotime->secs localization-load-time))
             "seconds")

    (println (count db)
             "records loaded in"
             (format "%.3f" (utils/nanotime->secs db-load-time))
             "seconds")
  ))
