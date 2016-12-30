(ns gd-edit.game-dirs
  (:require [clojure.java.io :as io]
            [gd-edit.utils :as u])

  (:import [com.sun.jna.platform.win32 WinReg Advapi32Util]))

(defn get-steam-path
  "Get steam installation path"
  []

  (try
    (Advapi32Util/registryGetStringValue WinReg/HKEY_CURRENT_USER
                                         "SOFTWARE\\Valve\\Steam"
                                         "SteamPath")
    (catch Exception e nil)))

(defn get-game-dir
  "Get the game installation path"
  []

  (if (= (System/getProperty "os.name") "Mac OS X")
    (u/expand-home "~/Dropbox/Public/GrimDawn")
    (str (get-steam-path) "\\steamapps\\common\\Grim Dawn")))

(defn get-steam-cloud-save-dir
  []

  (if (= (System/getProperty "os.name") "Mac OS X")
    ""

    (let [userdata-dir (io/file (get-steam-path) "userdata")
          user-profile-dir (->> (io/file userdata-dir)
                                (.listFiles)
                                (filter #(.isDirectory %1))
                                (first))]
      (.getPath (io/file user-profile-dir "219990\\remote\\save\\main")))))

(defn get-local-save-dir
  []

  (if (= (System/getProperty "os.name") "Mac OS X")
    (u/expand-home "~/Dropbox/Public/GrimDawn/main")

    (let [user-dir (System/getProperty "user.home")]
      (.getPath (io/file user-dir "My Documents\\My Games\\Grim Dawn\\save\\main")))))

(defn get-save-dirs
  []

  (if (= (System/getProperty "os.name") "Mac OS X")
    [(get-local-save-dir)]
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

(defn get-all-save-file-dirs
  []

  (->> (get-save-dirs)
       (map io/file)
       (map #(.listFiles %1))
       (apply concat)
       (filter #(.isDirectory %1))
       (filter #(.exists (io/file %1 "player.gdc")))))
