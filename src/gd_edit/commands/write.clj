(ns gd-edit.commands.write
  "Deals with writing the character file to disk."
  (:require [gd-edit.app-util :as au]
            [gd-edit.utils :as u]
            [clojure.java.io :as io]
            [jansi-clj.core :refer [red green yellow]]
            [clojure.string :as str]
            [gd-edit.globals :as globals]
            [me.raynes.fs :as fs]
            [gd-edit.io.gdc :as gdc]
            [gd-edit.io.stash :as stash]))


(defn- get-loadpath
  [character]
  (:meta-character-loaded-from character))

(defn- get-savepath
  [character]

  (let [loadpath (get-loadpath character)
        loadpath-components (u/filepath->components (str loadpath))]

    (u/components->filepath
     (assoc loadpath-components
            (- (count loadpath-components) 2)
            (str "_" (:character-name character))))))

(defn- rename-save-dir-if-required
  [character]

  (let [loadpath (get-loadpath character)
        savepath (get-savepath character)]

    (if (= loadpath savepath)
      :rename-not-needed

      (if (.renameTo (-> (io/file loadpath)
                         (.getParentFile))
                     (-> (io/file savepath)
                         (.getParentFile)))
        :rename-success
        :rename-failed))))

(defn- get-new-backup-path
  [filepath]

  ;; Grab all sibling files of the given file
  (let [sibling-files (-> (io/file filepath)
                          (.getParentFile)
                          (.listFiles))

        ;; What's the filename?
        filename (u/last-path-component filepath)

        ;; Filter for all items that starts with the filename followed by .bakXXX
        save-backups (->> sibling-files
                          (filter #(.isFile %1))
                          (filter #(str/starts-with? (.getName %1) filename))
                          (filter #(re-matches #"\.bak.*$" (subs (.getName %) (count filename)))))]

    (format "%s.bak%d" filepath (inc (count save-backups)))))

(defn- backup-file
  "Renames the given file to a .bakXXX file if it exists."
  [filepath]

  (let [f (io/file filepath)
        backup-path (get-new-backup-path filepath)]

    (if (.exists f)
      [(.renameTo f (io/file backup-path)) backup-path]
      [:nothing-to-backup backup-path])))

(defn- write-character-file-after-backup
  [character]

  (let [savepath (get-savepath character)

        ;; save the file
        [backup-status backup-path] (backup-file savepath)]
    (cond
      (= backup-status true)
      (do
        (println "Save file backed up to:")
        (u/print-indent 1)
        (println (yellow backup-path)))

      (= backup-status false)
      (do
        (println "Cannot backup file up to:")
        (u/print-indent 1)
        (println (yellow backup-path))))

    (println "Saving file:" )
    (u/print-indent 1)
    (println (yellow savepath))

    (gdc/write-character-file character (.getCanonicalPath (io/file savepath)))))

(defn- write-character-file
  [character]

  (let [character-name (:character-name character)

        ;; If the save directory should be renamed (because character name changed),
        ;; do so now...
        rn-status (rename-save-dir-if-required character)]

    ;; Print out any rename status
    (cond
      (= rn-status :rename-success)
      (println "Renamed save directory to match character name: " character-name)

      ;; Did the rename fail because there because the target directory already exists?
      ;; If so, inform the user...
      (= rn-status :rename-failed)
      (if (.exists (.getParentFile (io/file (get-savepath character))))
        (do
          (println "Unable to rename save directory because it conflicts with an existing directory: ")
          (u/print-indent 1)
          (println (yellow (.getCanonicalPath (.getParentFile (io/file (get-savepath character))))))
          (newline)
          (println "Please rename your character before trying again."))

        ;; If we don't know why the renamed failed, just print a generic message.
        (println "Unable to rename save directory to match character name: " character-name)))

    ;; If rename failed (rename required, but could not be done...)
    ;; Don't write the file and just return the character as is
    (if (= rn-status :rename-failed)
      character

      ;; Otherwise, actually write out the character file
      ;; Update the last loaded location in case it changed
      (do
        (write-character-file-after-backup character)
        (assoc character :meta-character-loaded-from (get-savepath character))))))

(defn write-loaded-character-file!
  []

  ;; Although we're writing out the file...
  ;; Some meta-data on the character may change due to character & savedir rename handling
  (reset! globals/character (write-character-file @globals/character)))

(defn write-separate-copy
  [character new-name]

  (let [from-char-dir-file (-> (get-savepath character)
                               (io/file)
                               (.getParentFile))

        modified-character (assoc character :character-name new-name)
        to-char-dir-file (-> (get-savepath modified-character)
                             (io/file)
                             (.getParentFile))]

    (if (.exists to-char-dir-file)
      [:new-path-already-exists (str to-char-dir-file)]

      (do
        ;; Recursively copy the original character directory
        (fs/copy-dir from-char-dir-file to-char-dir-file)

        ;; Write out the character to the new location
        (gd-edit.io.gdc/write-character-file modified-character
                                                 (-> modified-character
                                                     (get-savepath)
                                                     (io/file)
                                                     (.getCanonicalPath)))
        [:done (str to-char-dir-file)]))))

(defn write-handler
  [[input tokens]]

  (cond
    (not (au/character-loaded?))
    (println "Don't have a character loaded yet!")

    ;; did the user issue a "write stash"?
    (u/case-insensitive= (first tokens) "stash")
    ;; Check if the stash is actually different from the old known state
    (let [new-state (@globals/character :transfer-stash)
          old-state (@globals/transfer-stash :stash)]

      ;; If so, tell the user we won't do anything
      (if (= new-state old-state)
        (println "Transfer stash not saved. Nothing changed.")

        ;; Otherwise, save the data back to where it was read from.
        (-> @globals/transfer-stash
            (assoc :stash new-state)
            (stash/write-stash-file (@globals/transfer-stash :meta-stash-loaded-from)))))


    ;; Make sure GD isn't running
    (au/is-grim-dawn-running?)
    (do
      (println "Please quit Grim Dawn before saving the file.")
      (println (red "File not saved!")))

    ;; If the user invoked the command without any parameters...
    ;; Write out the currently loaded character
    (nil? (first tokens))
    (write-loaded-character-file!)

    ;; The user supplied some parameter with the command...
    ;; Try to write out a new copy of the character
    :else
    (let [[status savepath] (write-separate-copy @globals/character (first tokens))]
      (cond
        (= status :new-path-already-exists)
        (do
          (println (red "Cannot copy character because the directory already exists:"))
          (u/print-indent 1)
          (println savepath))

        :else
        (do
          (println (green "Ok!"))
          (newline)
          (println "If you're running steam with cloud saves, please remember to restart steam.")
          (println "Otherwise, your copied character will not show up in the character selection menu."))))))
