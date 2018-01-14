(ns gd-edit.game-dirs
  (:require [clojure.java.io :as io]
            [gd-edit.utils :as u]
            [gd-edit.globals :as globals])

  (:import [com.sun.jna.platform.win32 WinReg Advapi32Util]))

(declare looks-like-game-dir)

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


;;------------------------------------------------------------------------------
;; Path construction & File fetching utils
;;------------------------------------------------------------------------------
(def database-file "database/database.arz")
(def templates-file "database/templates.arc")
(def localization-file "resources/Text_EN.arc")
(def texture-file "resources/Items.arc")

(defn get-game-dir
  ([]
   (get-game-dir (get-game-dir-search-list)))

  ([game-dirs]
   (->> game-dirs
        (filter looks-like-game-dir)
        (first))))

(defn get-gdx1-dir
  []
  "Returns the directory for Ashes of Malmouth dlc."
  (io/file (get-game-dir) "gdx1"))

(defn get-mod-dir
  "Returns the configured mod's directory"
  []
  (:moddir @globals/settings))

(defn get-file-override-dirs
  "Returns a list of directories to look for game asset files in"
  []
  [(get-game-dir)
   (get-gdx1-dir)
   (get-mod-dir)])

(defn get-db-file-overrides
  []
  (->> [(io/file (get-game-dir) database-file)
        (io/file (get-gdx1-dir) "database/GDX1.arz")
        (io/file (get-mod-dir) database-file)]
       (filter u/path-exists?)
       (into [])))

(defn get-file-and-overrides
  "Given the relative path of a game asset file, return a vector of all matched files.

  For example, each mod is likely to have a database file and a localization file.
  When we're processing a the database file, then, it's not enough to just process
  the base game's database file, but all active mods also.

  This function builds such a list for callers to process."
  [relative-path]

  (if (= relative-path database-file)
    (get-db-file-overrides)

    (->> (get-file-override-dirs)
         (filter u/path-exists?)
         (map #(io/file % relative-path))
         (into []))))

(defn looks-like-game-dir
  [path]

  (if (and (u/path-exists? (io/file path database-file))
           (u/path-exists? (io/file path localization-file)))
    true
    false))
