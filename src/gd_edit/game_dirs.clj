(ns gd-edit.game-dirs
  (:require [clojure.java.io :as io]
            [gd-edit.utils :as u]
            [gd-edit.globals :as globals]
            [gd-edit.vdf-parser :as vdf]
            [com.rpl.specter :as specter]
            [taoensso.timbre :as t]
            [clojure.string :as str])
  (:import [com.sun.jna.platform.win32 WinReg Advapi32Util]))

(declare looks-like-game-dir)

(defn- parse-int
  "Try to parse the given string as an int. Returns Integer or nil."
  [s]
  (try (Integer/parseInt s)
       (catch Exception _ nil)))

(defn get-steam-path
  "Get steam installation path"
  []

  (u/log-exceptions-with t/debug
   (Advapi32Util/registryGetStringValue WinReg/HKEY_CURRENT_USER
                                        "SOFTWARE\\Valve\\Steam"
                                        "SteamPath")))

(defn get-steam-library-folders
  "Retrieves the library folders as defined in steamapps/libraryfolders.vdf"
  []
  (let [lib-folder-file (io/file (get-steam-path) "steamapps/libraryfolders.vdf")]
    (when (.exists lib-folder-file)
      (as-> lib-folder-file $
        (vdf/parse $)
        (get $ "LibraryFolders")
        (filter #(parse-int (key %)) $)
        (specter/transform [specter/ALL specter/FIRST] parse-int $)

        ;; Sort by the directory priority
        (sort-by first $)
        ;; Grab the directories only
        (map second $)))))


(defn- standard-game-dirs
  "Get the game's expected installation paths"
  []

  (cond
    (u/running-osx?)
    [(u/expand-home "~/Dropbox/Public/GrimDawn")]

    (u/running-linux?)
    [""]

    :else
    (->> (concat [(get-steam-path)] (get-steam-library-folders))
         (remove nil?)
         (map #(str % "\\steamapps\\common\\Grim Dawn")))))

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
   (clean-list (into [(get settings :game-dir)] (standard-game-dirs)))))

(defn get-steam-cloud-save-dirs
  []

  (when (u/running-windows?)
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
    (.getPath (io/file (u/home-dir) "Documents\\My Games\\Grim Dawn\\save\\main"))))

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
  (let [p (u/filepath->components (str save-dir))
        target-idx (.lastIndexOf p "main")
        p (if (not= target-idx -1)
            (assoc p target-idx "user")
            p)]
    (cond-> (->> p
                 u/components->filepath
                 io/file)
      (not= java.io.File (type save-dir))
      (.getAbsolutePath))))

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
  "Returns the directory for Ashes of Malmouth dlc."
  []
  (io/file (get-game-dir) "gdx1"))

(defn get-gdx2-dir
  "Returns the directory for Forgotten Gods dlc."
  []
  (io/file (get-game-dir) "gdx2"))

(defn get-mod-dir
  "Returns the configured mod's directory"
  []
  (:moddir @globals/settings))

(defn get-file-override-dirs
  "Returns a list of directories to look for game asset files in"
  []
  [(get-game-dir)
   (get-gdx1-dir)
   (get-gdx2-dir)
   (get-mod-dir)])

(defn files-with-extension
  [directory ext]
  (->> (io/file directory)
       file-seq
       (filter #(and (.isFile %)
                     (u/case-insensitive= (u/file-extension %) ext)))))

(defn get-mod-db-file
  [mod-dir]

  (when mod-dir
    (let [components (u/path-components (str mod-dir))
          mod-name (last components)]

      (->> (file-seq (io/file mod-dir "database"))
           (filter #(u/case-insensitive= (u/path-basename %) mod-name))
           first))))

(defn get-db-file-overrides
  []
  (->> (concat [(io/file (get-game-dir) database-file)
                (io/file (get-gdx1-dir) "database/GDX1.arz")
                (io/file (get-gdx2-dir) "database/GDX2.arz")]
               [(get-mod-db-file (get-mod-dir))])
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
         (map #(io/file % relative-path))
         (filter #(u/path-exists? %))
         (into []))))

(defn looks-like-game-dir
  [path]

  (if (and (u/path-exists? (io/file path database-file))
           (u/path-exists? (io/file path localization-file)))
    true
    false))

(defn is-cloud-save-dir?
  [dir]
  (->> (get-steam-cloud-save-dirs)
       (map #(.getParent (io/file %)))
       (some #(str/starts-with? dir %))))

(defn is-mod-save-dir?
  [dir]
  (->> (get-save-dir-search-list)
       (map save-dir->mod-save-dir)
       (some #(str/starts-with? dir %))))

(defn is-character-from-cloud-save?
  [character]
  (is-cloud-save-dir? (:meta-character-loaded-from character)))

(defn is-character-from-mod-save?
  [character]
  (is-mod-save-dir? (:meta-character-loaded-from character)))

(defn save-dir-type
  [dir]

  (let [cloud? (is-cloud-save-dir? dir)
        custom? (is-mod-save-dir? dir)
        builder (StringBuilder.)]
    (if cloud?
      (.append builder "cloud")
      (.append builder "local"))
    (when custom?
      (.append builder " custom"))
    (.toString builder)))

;;------------------------------------------------------------------------------
;; Transfer stash
;;------------------------------------------------------------------------------
(defn get-transfer-stash
  [character]

  (when-not (empty? character)
    (let [target-file (if (:hardcore-mode character)
                        "transfer.gsh"
                        "transfer.gst")
          target-dir (cond->>
                         ;; Grab the "top" of the save directory
                         (->> (get-save-dir-search-list)
                              (map #(.getParentFile (io/file %))))

                       ;; If a mod is active, navigate to its directory
                       (and (is-character-from-mod-save? character)
                            (not-empty(get-mod-dir)))
                       (map #(io/file % (u/last-path-component (:moddir @globals/settings))))

                       ;; Grab the first item that has the stash file we're looking for
                       :then
                       (some #(when (or (.exists (io/file % "transfer.gst"))
                                        (.exists (io/file % "transfer.gsh")))
                                %)))]
      (io/file target-dir target-file))))
