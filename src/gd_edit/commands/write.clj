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
            [gd-edit.io.stash :as stash]
            [gd-edit.game-dirs :as dirs]
            [gd-edit.db-utils :as dbu]
            [clojure.data.csv :as csv]
            ))


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

    (if (= (str loadpath) (str savepath))
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

(defn- cycle-backup-file
  [filepath max-backup-count]

  (let [regex #"\.bak([0-9]+)$"

        sibling-files (-> (io/file filepath)
                          (.getParentFile)
                          (.listFiles))

        filename (u/last-path-component (str filepath))

        save-backups (->> sibling-files
                          (filter #(.isFile %1))
                          (filter #(str/starts-with? (.getName %1) filename))
                          (filter #(second (re-matches regex (subs (.getName %) (count filename))))))

        missing-backups (->> save-backups
                             (map #(->> (re-matches regex (subs (.getName %) (count filename)))
                                        second
                                        (Integer/parseInt)))
                             (into #{})
                             (clojure.set/difference (into #{} (range 1 (inc max-backup-count))))
                             (sort))

        backup-path (if (not-empty missing-backups)
                      (io/file (format "%s.bak%d" filepath (first missing-backups)))
                      (first (sort-by #(.lastModified %) save-backups)))]

    (when (.exists backup-path)
      (.delete backup-path))
    [(.renameTo (io/file filepath) (io/file backup-path)) backup-path]))

(defn- write-character-file-after-backup
  [character]

  (let [savepath (get-savepath character)

        ;; save the file
        [backup-status backup-path] (backup-file savepath)]
    (cond
      (= backup-status true)
      (do
        (u/print-line "Save file backed up to:")
        (u/print-indent 1)
        (u/print-line (yellow backup-path)))

      (= backup-status false)
      (do
        (u/print-line "Cannot backup file up to:")
        (u/print-indent 1)
        (u/print-line (yellow backup-path))))

    (u/print-line "Saving file:" )
    (u/print-indent 1)
    (u/print-line (yellow savepath))

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
      (u/print-line "Renamed save directory to match character name: " character-name)

      ;; Did the rename fail because there because the target directory already exists?
      ;; If so, inform the user...
      (= rn-status :rename-failed)
      (if (.exists (.getParentFile (io/file (get-savepath character))))
        (do
          (u/print-line "Unable to rename save directory because it conflicts with an existing directory: ")
          (u/print-indent 1)
          (u/print-line (yellow (.getCanonicalPath (.getParentFile (io/file (get-savepath character))))))
          (u/newline-)
          (u/print-line "Please rename your character before trying again."))

        ;; If we don't know why the renamed failed, just print a generic message.
        (u/print-line "Unable to rename save directory to match character name: " character-name)))

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
  [character new-name mod?]

  (let [from-char-dir-file (-> (get-savepath character)
                               (io/file)
                               (.getParentFile))

        modified-character (assoc character :character-name new-name)

        ;; Change the character file directory
        to-char-dir-file (cond-> (get-savepath modified-character)
                           mod?
                           dirs/save-dir->mod-save-dir

                           :then
                           (io/file)

                           :then
                           (.getParentFile))]

    (if (.exists to-char-dir-file)
      [:new-path-already-exists (str to-char-dir-file)]

      (do
        ;; Recursively copy the original character directory
        (fs/copy-dir from-char-dir-file to-char-dir-file)

        ;; Write out the character to the new location
        (gd-edit.io.gdc/write-character-file modified-character
                                             (io/file to-char-dir-file "player.gdc"))
        [:done (str to-char-dir-file)]))))

(defn write-handler
  [[input tokens]]

  (cond
    (not (au/character-loaded?))
    (u/print-line "Don't have a character loaded yet!")

    ;; Make sure GD isn't running
    (and
     (dirs/is-character-from-cloud-save? @globals/character)
     (au/is-grim-dawn-running?))
    (do
      (u/print-line "Please quit Grim Dawn before saving the file.")
      (u/print-line (red "File not saved!")))

    ;; If the user invoked the command without any parameters...
    ;; Write out the currently loaded character
    (nil? (first tokens))
    (write-loaded-character-file!)

    ;; The user supplied some parameter with the command...
    ;; Try to write out a new copy of the character
    :else
    (let [[new-character-name mod?] tokens

          mod? (if (= "mod" mod?)
                 true
                 false)

          ;; The user asked to copy a character as a mod character?
          ;; We need to make some changes to the character for this to work...
          _ (when mod?
              ;; Always reset the in-main-quest flag.
              ;; This appears to allow whatever carry quest progress into the mod.
              (swap! globals/character #(update % :in-main-quest (constantly false))))

          [status savepath] (write-separate-copy @globals/character new-character-name mod?)]
      (cond
        (= status :new-path-already-exists)
        (do
          (u/print-line (red "Cannot copy character because the directory already exists:"))
          (u/print-indent 1)
          (u/print-line savepath))

        :else
        (do
          (u/print-line (green "Ok!"))
          (when (dirs/is-character-from-cloud-save? @globals/character)
            (u/print-line "Looks like you're steam with cloud saves, please remember to restart steam.")
            (u/print-line "Otherwise, your copied character will not show up in the character selection menu.")))))))

(defn cycle-backup-and-save-transfer-stash
  [stash]
  (let [save-path (:meta-stash-loaded-from stash)
        [backup-status backup-path] (cycle-backup-file save-path 3)]

    (cond
      (= backup-status true)
      (do
        (u/print-line "Save file backed up to:")
        (u/print-indent 1)
        (u/print-line (yellow backup-path)))

      (= backup-status false)
      (do
        (u/print-line "Cannot backup file up to:")
        (u/print-indent 1)
        (u/print-line (yellow backup-path))))

    (u/print-line "Saving transfer stash file:" )
    (u/print-indent 1)
    (u/print-line (yellow save-path))

    (stash/write-stash-file stash (@globals/transfer-stash :meta-stash-loaded-from))))

(defn write-stash-handler
  [[_ _]]

  (cond
    (nil? (@globals/character :transfer-stashes))
    (u/print-line "The transfer stash had not been loaded")

    ;; Editing the transfer stash in cloud saves is not supported
    (dirs/is-character-from-cloud-save? @globals/character)
    (do
      (u/print-line "Sorry, saving the transfer stash in cloud saves is not supported.")
      (u/print-line (red "File not saved!")))

    :else
    ;; Check if the stash is actually different from the old known state
    (let [new-state (@globals/character :transfer-stashes)
          old-state (@globals/transfer-stash :stash)]

      ;; If so, tell the user we won't do anything
      (if (= new-state old-state)
        (u/print-line "Transfer stash not saved. Nothing changed.")

        (do
          ;; Otherwise, save the data back to where it was read from.
          (-> @globals/transfer-stash
              (assoc :stash new-state)
              (cycle-backup-and-save-transfer-stash))
          (u/print-line (green "Ok!")))))))


(defn row-maps->csv-vectors
  ([row-maps]
   (row-maps->csv-vectors row-maps
                          (->> row-maps
                               first
                               keys)))

  ([row-maps column-order]
   (map #(mapv % column-order) row-maps)))

(defn write-character-list
  [[input tokens]]


  (let [output-filename (or (first tokens)
                            "character-list.csv")
        loc-table (dbu/localization-table)
        target-fields [:character-name :player-class-name :character-level :hardcore-mode]
        chars-list (->> (au/character-list)  ;; Grab a list of all known characters

                        ;; Take each character, grab  [name, class, level]
                        (map (fn [char-entry]
                               (-> char-entry
                                   :gdc-path
                                   gdc/load-character-file
                                   (select-keys target-fields))))

                        ;; Convert the class name from a tag name to a display name
                        (map (fn [char-data]
                               (update char-data :player-class-name loc-table)))

                        )
        ]


    ;; Write the character list to a CSV file
    (with-open [out-file (io/writer output-filename)]
      (let [columns target-fields
            headers ["name" "class" "level" "hardcore"]
            rows (row-maps->csv-vectors chars-list columns)]
        (csv/write-csv out-file
                       (cons headers rows))))))
