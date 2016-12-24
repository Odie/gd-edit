(ns gd-edit.game-dirs
  (:require [clojure.java.io :as io]
            [gd-edit.utils :as u]))

(defn get-game-dir
  []

  (if (= (System/getProperty "os.name") "Mac OS X")
    (u/expand-home "~/Dropbox/Public/GrimDawn")
    "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Grim Dawn"))

(defn get-steam-cloud-save-dir
  []
  (let [userdata-dir "C:\\Program Files (x86)\\Steam\\userdata\\"
        user-profile-dir (->> (io/file userdata-dir)
                              (.listFiles)
                              (filter #(.isDirectory %1))
                              (first))]
    (.getPath (io/file user-profile-dir "219990\\remote\\save\\main"))))

(defn get-local-save-dir
  []

  (let [user-dir (System/getProperty "user.home")]
    (.getPath (io/file user-dir "My Documents\\My Games\\Grim Dawn\\save\\main"))))

(defn get-save-dirs
  []

  (if (= (System/getProperty "os.name") "Mac OS X")
    [(u/expand-home "~/Dropbox/Public/GrimDawn/main")]
    [(get-local-save-dir) (get-steam-cloud-save-dir)]))

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
