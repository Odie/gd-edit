(ns gd-edit.game-dirs
  (:require [clojure.java.io :as io]
            [gd-edit.utils :as u]))

(defn get-game-dir
  []

  (if (= (System/getProperty "os.name") "Mac OS X")
    (u/expand-home "~/Dropbox/Public/GrimDawn")
    "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Grim Dawn"))

(defn get-save-dir
  []

  (if (= (System/getProperty "os.name") "Mac OS X")
    (u/expand-home "~/Dropbox/Public/GrimDawn/main")
    "C:\\Program Files (x86)\\Steam\\userdata\\219990\\remote\\save\\main"))

(defn get-db-filepath
  []

  (-> (get-game-dir)
      (io/file "database" "database.arz")
      (.getPath)))

(defn get-localization-filepath
  []

  (-> (get-game-dir)
      (io/file "resources" "text_en.arc")
      (.getPath)))
