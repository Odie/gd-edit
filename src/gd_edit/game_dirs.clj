(ns gd-edit.game-dirs
  (:require [clojure.java.io :as io]
            [gd-edit.utils :as u]
            [gd-edit.globals :as globals])

  (:import [com.sun.jna.platform.win32 WinReg Advapi32Util]))

(defn get-steam-path
  "Get steam installation path"
  []

  (u/log-exceptions
    (Advapi32Util/registryGetStringValue WinReg/HKEY_CURRENT_USER
                                         "SOFTWARE\\Valve\\Steam"
                                         "SteamPath")))

(defn- standard-game-dirs
  "Get the game's expected installation path"
  []

  (cond
    (u/running-osx?)
    (u/expand-home "~/Dropbox/Public/GrimDawn")

    (u/running-linux?)
    ""

    :else
    (str (get-steam-path) "\\steamapps\\common\\Grim Dawn")))

(defn- clean-list
  "Removes nil from collection and return a list"
  [coll]

  (->> coll
       (remove nil?)
       (into [])))

(defn get-game-dir-search-list
  "Returns all possible locations where the game dir might be found.

  Note that this function respects the :game-dir setting in the user's settings.edn file."
  ([]
   (get-game-dir-search-list @globals/settings))

  ([settings]
   (clean-list [(get settings :game-dir) (standard-game-dirs)])))

(defn get-steam-cloud-save-dirs
  []

  (cond
    (u/running-nix?)
    ""

    :else
    (let [userdata-dir (io/file (get-steam-path) "userdata")]
      (->> (io/file userdata-dir)
           (.listFiles)
           (filter #(.isDirectory %1))
           (map #(io/file % "219990\\remote\\save\\main"))
           (filter #(.exists %))
           (map #(.getPath %))))))

(declare get-game-dir)

(defn get-local-save-dir
  []

  (cond
    (u/running-osx?)
    (u/expand-home "~/Dropbox/Public/GrimDawn/main")

    (u/running-linux?)
    ""

    :else
    (let [user-dir (System/getProperty "user.home")]
      (.getPath (io/file user-dir "Documents\\My Games\\Grim Dawn\\save\\main")))))

(defn get-save-dir-search-list
  "Returns all possible locations where the save dir might be found.

  Note that this function respects the :save-dir setting in the user's setting.edn file."
  []

  (cond
    (u/running-osx?)
    (clean-list [(get @globals/settings :save-dir) (get-local-save-dir)])
    :else
    (clean-list (concat [(get @globals/settings :save-dir) (get-local-save-dir)] (get-steam-cloud-save-dirs)))))

(defn save-dir->mod-save-dir
  [save-dir]
  (-> (io/file save-dir)
      (.getParentFile)
      (io/file "user")
      (.getAbsolutePath)))

(defn get-all-save-file-dirs
  []
  (->> (map save-dir->mod-save-dir (get-save-dir-search-list))
       (concat (get-save-dir-search-list))
       (map io/file)
       (map #(.listFiles %1))
       (apply concat)
       (filter #(.isDirectory %1))
       (filter #(.exists (io/file %1 "player.gdc")))))

(defn make-db-filepath
  [game-dir]

  (io/file game-dir "database" "database.arz"))

(defn- make-templates-filepath
  [game-dir]

  (io/file game-dir "database" "templates.arc"))

(defn make-localization-filepath
  [game-dir]

  (io/file game-dir "resources" "Text_EN.arc"))

(defn make-mod-db-filepath
  [mod-dir]
  (let [mod-name (u/last-path-component mod-dir)]
    (io/file mod-dir "database" (str mod-name ".arz"))))

(defn get-db-filepath
  "Checks through all game dirs and retrieves the first db file"
  []

  (make-db-filepath (get-game-dir)))

(defn get-templates-filepath
  "Checks through all game dirs and retrieves the first templates file"
  []

  (make-templates-filepath (get-game-dir)))

(defn get-localization-filepath
  "Checks through all game dirs and retrieves the first English localization file"
  []

  (make-localization-filepath (get-game-dir)))

(defn get-resources-file
  [filename]

  (io/file (get-game-dir) "resources" filename))

(defn looks-like-game-dir
  [path]

  (if (and (u/path-exists (make-db-filepath path))
           (u/path-exists (make-localization-filepath path)))
    true
    false))

(defn get-game-dir
  ([]
   (get-game-dir (get-game-dir-search-list)))

  ([game-dirs]
   (->> game-dirs
        (filter looks-like-game-dir)
        (first))))
