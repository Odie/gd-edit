(ns gd-edit.self-update
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.core.async :as async :refer [thread >!!]]
            [progress.file :as progress]
            [gd-edit
             [utils :as utils]]
            [me.raynes.fs :as fs]
            [clj-http.client :as client])
  (:import java.io.IOException))

(defn fetch-url [url-str]
  (let [response (client/get url-str
                             {:headers {"User-Agent" "curl/7.43.0"}})]
    (if (not= (response :status) 200)
      (throw (IOException. (str "Got response status:" (response :status))))

      (response :body))))

(defn fetch-url-as-stream [url-str]
  (let [response (client/get url-str
                             {:headers {"User-Agent" "curl/7.43.0"}
                              :as :stream})]
    (if (not= (response :status) 200)
      (throw (IOException. (str "Got response status:" (response :status))))

      (response :body))))

(defn get-build-info
  "Get the build info associated with the current build"
  []
  (when-let [info-file (io/resource "build.edn")]
    (edn/read-string (slurp info-file))))

(defn is-newer?
  "Check if build-a is newer than build-b"
  [build-a build-b]

  (.isAfter (.toInstant (:timestamp build-a))
            (.toInstant (:timestamp build-b))))

(defn fetch-latest-build-info
  "Get the information about the latest available build from network
  WARNING: This may block for a long time."
  []
  (let [url (cond
              (utils/running-nix?)
              "http://tiny.cc/gdednixedn"

              :else
              "http://tiny.cc/gdededn")]

    ;; Return the contents of the file to the caller
    (edn/read-string (fetch-url url))))

(defn fetch-latest-build
  [build-info]
  (let [url (cond
              (utils/running-nix?)
              "http://tiny.cc/gdednixbin"

              :else
              "http://tiny.cc/gdedexe")
        file (java.io.File/createTempFile "gd-edit" ".exe")]

    ;; Try to copy the file contents to a temp location
    (println "Downloading new version")

    (progress/with-file-progress file :filesize (:filesize build-info)
      (io/copy (fetch-url-as-stream url) file))

    ;; Return the temp location to caller
    file))

(defn fetch-has-new-version?
  "Attempts to fetch the latest version of the program.
  Returns either:
    :up-to-date - if already running the latest version
    :new-version-available - if a later version is available online

  Can throw exception. We're performing various networking tasks. Any number of conditions may arise.
  It's not clear what exactly might be thrown here.
  It's up to the caller to handle network error conditions."
  []

  (when-let [latest-build-info (fetch-latest-build-info)]
    (when-let [current-build-info (get-build-info)]
      ;; (println "latest: " latest-build-info)
      ;; (println "current: " current-build-info)

      ;; Is the latest build newer than the one we're running?
      (if-not (is-newer? latest-build-info current-build-info)
        [:up-to-date]
        [:new-version-available latest-build-info]))))

(defn fetch-latest-version
  "Attempts to fetch the latest version of the program.
  Returns either:
    :up-to-date - if already running the latest version
    tempfile - if can be

  Can throw exception. We're performing various networking tasks. Any number of conditions may arise.
  It's not clear what exactly might be thrown here.
  It's up to the caller to handle network error conditions."
  []

  (let [[new-ver-status latest-build-info](fetch-has-new-version?)]
    (if (=  new-ver-status :new-version-available)

      ;; Try to fetch the latest version
      (fetch-latest-build latest-build-info)

      ;; Or say that everything is up to date
      :up-to-date)))

(defn- get-restart-script-file
  []
  (io/file (utils/working-directory) "restart.bat"))

(defn- restart-for-win
  [new-exe]

  (println "Restarting...")
  (let [running-exe-path (io/file (System/getProperty "java.class.path"))
        backup-path (io/file (str running-exe-path ".bak"))
        restart-script-path (get-restart-script-file)]

    ;; Write out the restart script
    (spit restart-script-path
     (utils/fmt
      "
        @echo off
        echo Preparing to restart gd-edit...

        REM remove old backup file
        del /F #{backup-path}
        if %%ERRORLEVEL%% neq 0 goto backup_error

        REM rename the original exe as a backup
        move #{running-exe-path} #{backup-path}
        if %%ERRORLEVEL%% neq 0 goto backup_error

        REM move the new exe to where the original exe was
        move #{(str new-exe)} #{running-exe-path}
        if %%ERRORLEVEL%% neq 0 goto replace_error

        REM finally, start the new version
        echo Restarting gd-edit...
        start #{running-exe-path}
        exit 0

        backup_error:
        echo Could not create backup file. Aborting self update.
        pause
        exit 1

        replace_error:
        echo Could not move new version to correct location.
        echo Attempting to restore backup.
        echo If this doesn't work, please manually rename
        echo #{backup-path} => #{running-exe-path}
        move #{backup-path} #{running-exe-path}
        start #{running-exe-path}
        pause
        exit 1
      "
      ))
    (.setExecutable restart-script-path true)

    ;; Start the restart script
    (-> (ProcessBuilder. ["cmd.exe" "/C" "start" (str restart-script-path)])
        (.start))))

(defn- restart-for-nix
  [new-exe]

  (let [running-exe-path (io/file (System/getProperty "java.class.path"))
        backup-path (io/file (str running-exe-path ".bak"))]


    ;; Make a backup of the current exe/binary
    (if-not (.renameTo (io/file running-exe-path) (io/file backup-path))
      (println "Sorry. Could not rename the current executable.")

      ;; Replace the current binary with the new one
      (if-not (.renameTo (io/file new-exe) (io/file running-exe-path))
        (println "Sorry. Could not rename the new version.")

        (do
          (.setExecutable (io/file running-exe-path) true)
          (println "Please restart the editor manually."))))))

(defn- restart-self
  [new-exe]

  (cond
    (utils/running-nix?)
    (restart-for-nix new-exe)

    :else
    (restart-for-win new-exe))

    (System/exit 0))

(defn cleanup-restart-script
  []
  (let [resart-script-file (get-restart-script-file)]
    (when (.exists resart-script-file)
      (io/delete-file (get-restart-script-file)))))

(defn try-self-update
  "Attempts to update the running executable to the latest version.
  Returns:
  :up-to-date if the running exe is the latest version
  attempts to download and restart the program otherwise"
  []
  (let [fetch-result (fetch-latest-version)]
    (cond
      (instance? java.io.File fetch-result)
      (restart-self fetch-result)

      :else
      fetch-result)))
