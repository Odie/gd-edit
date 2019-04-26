(ns gd-edit.quest
  (:require clojure.data
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.rpl.specter :as specter]
            [gd-edit.game-dirs :as dirs]
            [gd-edit.globals :as globals]
            [gd-edit.io.arc :as arc]
            [gd-edit.io.gdd :as gdd]
            [gd-edit.io.qst :as qst]
            [gd-edit.utils :as u])
  (:import java.nio.ByteBuffer))

(defn quest-files
  "Given a path to a save file (gdc), retrieve all associated quest progress files."
  [savepath]

  (->> savepath
       (io/file)
       (.getParentFile)
       (file-seq)
       (filter #(str/ends-with? % "quests.gdd") )))

(defn quest-files->descriptor
  "Given a list of quest file/paths, turn them into maps with some meaningful fields."
  [quest-files basedir]

  (->> quest-files
       (map (fn [quest-file]
              (let [full-path (str quest-file)
                    relative-path (-> full-path
                                      (subs (inc(count (str basedir)))))
                    components (u/filepath->components relative-path)]
                {:path (str full-path)
                 :relpath relative-path
                 :map (first components)
                 :difficulty (str/lower-case (second components))})))))

(defn load-quest-files
  "Given a path to a character save path, return all associated quest progress data for
  the main game."
  [savepath]

  (as-> (quest-files savepath) $

    ;; Turn the paths into maps we can work with
    (quest-files->descriptor $ (.getParentFile (io/file savepath)))

    ;; We're only interested quest progress for the main game map.
    (filter #(= "levels_world001.map" (:map %)) $)

    ;; Load the quest progress
    (map (fn [item]
           [(:difficulty item) (gdd/load-quest-file (:path item))])
         $)

    ;; The loaded progress into a map keyed by difficulty
    (into {} $)))


(defn quest-def-files
  []

  (dirs/get-file-and-overrides "resources/Quests.arc"))

(defn- load-quest-defs-in-arc
  "Given the path to a arc-file that defines quests, return all records
  as maps."
  [arc-file]

  (->> ;; load contents as {recordname, content} maps
       (arc/load-arc-file arc-file)

       ;; Read in each record and return it as a map
       (map (fn [{:keys [recordname contents]}] (merge {:recordname recordname}
                                                      (->> contents
                                                           (ByteBuffer/wrap)
                                                           (qst/load-quest)
                                                           (:quest)))))))

(defn load-quest-defs
  "Load all known quest definitions for the game.

  Note that we're loading the defs from more than one file. The definitions
  do *not* override each other right now. Overriding or patching quests
  this way doesn't make a whole lot of sense.

  If a quest is broken, the definition would be patched. A mod is more likely
  to directly add a new quest than to patch an existing one this way.

  So uh... we just merge the definition collections without any additional
  processing."
  []

  (->> (quest-def-files)
       (map load-quest-defs-in-arc)
       (reduce concat)))

(defn quest-uid-index
  [quest-defs]

  (->> (qst/locate-quest-uids quest-defs)
       (group-by #(:uid (second %)))))

(defn annotate-quest-progress
  "Add readable :name fields to quest and tasks"
  [quest-progress quest-defs]

  (let [uid-index (quest-uid-index quest-defs)]
    ;; Annotate the quest progress with readable names
    (->> quest-progress

         ;; Attach names to all tasks
         (specter/transform [:quests specter/ALL :tasks specter/ALL]
                            #(qst/quest-item-attach-name uid-index %))

         ;; Attach names to all quests
         (specter/transform [:quests specter/ALL]
                            #(qst/quest-item-attach-name uid-index %))
         )
    )
  )


(defn load-annotated-quest-progress
  "Given a save file, load and annotate all the quest progress for the main game."
  [savepath]

  (let [quest-progress (load-quest-files savepath)
        quest-defs (load-quest-defs)]

    (->> quest-progress
         ;; Annotate progress with names
         (specter/transform [specter/MAP-VALS]
                            #(annotate-quest-progress % quest-defs)))))

(comment
 (def t
   (->> (load-quest-defs)
        (map #(map :recordname %))))

 (->> t
      (map set)
      (reduce clojure.set/union)
      (count)
      )

 (def t (load-quest-defs))

 (def p (get-in @globals/character [:quest "Elite"]))

 (annotate-quest-progress p t)

 )
