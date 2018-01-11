(ns gd-edit.qst-reader
  (:require [gd-edit.structure :as s]
            [gd-edit.utils :as utils]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [gd-edit.utils :as u]
            [gd-edit.globals :as globals]
            [gd-edit.gdc-reader :as gdc]
            [clojure.inspector]
            [com.rpl.specter :as specter]
            [clojure.string :as str])
  (:import  [java.nio ByteBuffer ByteOrder]
            [java.io FileOutputStream]))

;;------------------------------------------------------------------------------
;; Quest File format defs
;;------------------------------------------------------------------------------
(def FilePreamble
  (s/ordered-map
   :magic   :int32
   :version :int32))

(def Condition-Header
  (s/ordered-map
   :version     :byte
   :comparision :int32))

(def Condition-On-Quest
  (into Condition-Header
        (s/ordered-map
         :static/type :on-quest
         :quest       (s/string :utf-8)
         :on          :bool)))

(def Condition-Quest-Task-Complete
  (into Condition-Header
        (s/ordered-map
         :static/type :quest-task-complete
         :quest       (s/string :utf-8)
         :task        :int32
         :complete    :bool)))

(def Condition-Has-Experience
  (into Condition-Header
        (s/ordered-map
         :static/type :has-experience
         :amount      :int32)))

(def Condition-Is-Level
  (into Condition-Header
        (s/ordered-map
         :static/type :is-level
         :level       :int32)))

(def Condition-Is-Difficulty
  (into Condition-Header
        (s/ordered-map
         :static/type :is-difficulty
         :difficulty  :int32)))

(def Condition-Is-Hardcore
  (into Condition-Header
        (s/ordered-map
         :static/type :is-hardcore
         :hardcore    :bool)))

(def Condition-Has-Faction
  (into Condition-Header
        (s/ordered-map
         :static/type :has-faction
         :faction     (s/string :utf-8)
         :value       :int32)))

(def Condition-Has-Token
  (into Condition-Header
        (s/ordered-map
         :static/type :has-token
         :token   (s/string :utf-8)
         :has     :bool)))

(def Condition-Has-Money
  (into Condition-Header
        (s/ordered-map
         :static/type :has-money
         :amount  :int32)))

(def Condition-Has-Item
  (into Condition-Header
        (s/ordered-map
         :static/type :has-item
         :dbr       (s/string :utf-8)
         :count     :int32
         :complete  :bool)))

(def Condition-Has-Killed
  (into Condition-Header
        (s/ordered-map
         :static/type :has-killed
         :monster-names (s/variable-count (s/string :utf-8))
         :count         :int32)))

(def Condition-Has-Quest-Objective
  (into Condition-Header
        (s/ordered-map
         :static/type   :has-quest-objective
         :quest-file    (s/string :utf-8)
         :task-uid      :int32
         :objective-uid :int32
         :satisfied     :bool)))

(def Lua-Args
  (s/ordered-map
   :type          :int32
   :bool-val      :bool
   :number-val    :float
   :string-val    (s/string :utf-8)))

(def Condition-Lua-Script
  (into Condition-Header
        (s/ordered-map
         :static/type   :has-lua-script
         :function-name (s/string :utf-8)
         :args          (s/variable-count Lua-Args))))

(def Condition-Knows-Player
  (into Condition-Header
        (s/ordered-map
         :known          :bool)))

(def Condition-On-Quest-Task
  (into Condition-Header
        (s/ordered-map
         :static/type   :on-quest-task
         :quest         (s/string :utf-8)
         :task          :int32
         :on            :bool)))

(def Condition-Used-Quest-Item
  (into Condition-Header
        (s/ordered-map
         :static/type   :used-quest-task
         :dbr           (s/string :utf-8)
         :count         :int32)))

(def Condition-Has-Killed-Proxy
  (into Condition-Header
        (s/ordered-map
         :static/type  :has-killed-proxy
         :proxy-names  (s/variable-count (s/string :utf-8))
         :count        :int32)))

(def Condition-Wait-For-Completion
  (into Condition-Header
        (s/ordered-map
         :static/type  :wait-for-completion)))

(def Condition-Server-Has-Token
  (into Condition-Header
        (s/ordered-map
         :static/type  :server-has-token
         :token        (s/string :utf-8)
         :has          :bool)))

(def Condition-Quest-Blocked
  (into Condition-Header
        (s/ordered-map
         :static/type  :quest-blocked
         :quest        (s/string :utf-8)
         :blocked      :bool)))

(def Condition-Anyone-Has-Token
  (into Condition-Header
        (s/ordered-map
         :static/type  :anyone-has-token
         :token        (s/string :utf-8)
         :has          :bool)))

(def Condition-Check-Random-Value
  (into Condition-Header
        (s/ordered-map
         :static/type  :check-random-value
         :name         (s/string :utf-8)
         :value        :int32)))

(def Condition-Owns-DLC
  (into Condition-Header
        (s/ordered-map
         :static/type  :owns-dlc
         :dlc-id       :int32)))

(def Condition-Has-Tribute
  (into Condition-Header
        (s/ordered-map
         :static/type  :has-tribute
         :amount       :int32)))

(def Condition-Earned-Devotion
  (into Condition-Header
        (s/ordered-map
         :static/type  :earned-devotion
         :amount       :int32)))

(def Condition-Has-Mastery
  (into Condition-Header
        (s/ordered-map
         :static/type  :has-mastery
         :mastery      :int32
         :has          :bool)))

(def condition-read-map
  (->> [Condition-On-Quest
        Condition-Quest-Task-Complete,
        Condition-Has-Experience
        Condition-Is-Level
        Condition-Is-Difficulty
        Condition-Is-Hardcore
        Condition-Has-Faction
        Condition-Has-Token
        Condition-Has-Money
        Condition-Has-Item
        Condition-Has-Killed
        Condition-Has-Quest-Objective
        Condition-Lua-Script
        Condition-Knows-Player
        Condition-On-Quest-Task
        Condition-Used-Quest-Item
        Condition-Has-Killed-Proxy
        Condition-Wait-For-Completion
        Condition-Server-Has-Token
        Condition-Quest-Blocked
        Condition-Anyone-Has-Token
        Condition-Check-Random-Value
        Condition-Owns-DLC
        Condition-Has-Tribute
        Condition-Earned-Devotion
        Condition-Has-Mastery]
       (map-indexed vector)
       (into {})))

(defn- read-condition
  "Reads a single QuestCondition"
  [bb context]
  (let [type (.getInt bb)
        spec (condition-read-map type)]
    (assert spec (u/fmt "Condition spec type #{type} is not valid"))
    (s/read-struct spec bb context {:type type})))

(defn- read-conditions
  "Reads a collection of QuestConditions
  See ScriptableConditionCollection"
  [bb context]

  ;; How many conditions are to be read?
  (let [cond-count (.getInt bb)]
    (if (zero? cond-count)
      {}

      (do
        {:oper (.getInt bb)
         :conditions (doall (for [idx (range cond-count)]
                             (read-condition bb context)))}))))

(def Action-Header
  (s/ordered-map
   :version   :byte))

(defn Action-Begin-Quest
  [bb context]
  (assert false "Action-Begin-Quest is deprecated"))

(def Action-Begin-Quest-Task
  (into Action-Header
        (s/ordered-map
         :static/type  :begin-quest-task
         :quest        (s/string :utf-8)
         :task         :int32)))

(def Action-Complete-Quest
  (into Action-Header
        (s/ordered-map
         :static/type  :complete-quest
         :quest        (s/string :utf-8))))

(def Action-Complete-Quest-Task
  (into Action-Header
        (s/ordered-map
         :static/type  :complete-quest-task
         :quest    (s/string :utf-8)
         :task     :int32)))

(def Action-Give-Money
  (into Action-Header
        (s/ordered-map
         :static/type  :give-money
         :amount   :int32)))

(def Action-Give-Faction
  (into Action-Header
        (s/ordered-map
         :static/type  :give-faction
         :faction      (s/string :utf-8)
         :amount       :int32)))

(def Action-Give-Item
  (into Action-Header
        (s/ordered-map
         :static/type  :give-item
         :dbr          (s/string :utf-8)
         :amount       :int32)))

(def Action-Give-Level
  (into Action-Header
        (s/ordered-map
         :static/type  :give-level
         :amount       :int32)))

(def Action-Give-Experience
  (into Action-Header
        (s/ordered-map
         :static/type  :give-experience
         :amount       :int32)))

(def Action-Give-Token
  (into Action-Header
        (s/ordered-map
         :static/type  :give-token
         :token        (s/string :utf-8))))

(def Action-Remove-Token
  (into Action-Header
        (s/ordered-map
         :static/type  :remove-token
         :token        (s/string :utf-8))))

(def Action-Lua-Script
  (into Action-Header
        (s/ordered-map
         :static/type   :lua-script
         :function-name (s/string :utf-8)
         :args          (s/variable-count Lua-Args))))

(def Action-Debug-Print
  (into Action-Header
        (s/ordered-map
         :static/type   :debug-print
         :text          (s/string :utf-8))))

(def Action-Play-Sound
  (into Action-Header
        (s/ordered-map
         :static/type   :play-sound
         :dbr           (s/string :utf-8))))

(def Action-Play-Animation
  (into Action-Header
        (s/ordered-map
         :static/type   :play-animation
         :animation     :int32)))

(def Action-Give-Skill-Point
  (into Action-Header
        (s/ordered-map
         :static/type   :give-skill-point
         :amount        :int32)))

(def Action-Open-Merchant-Window
  (into Action-Header
        (s/ordered-map
         :static/type   :open-merchant-window)))

(def Action-Notification
  (into Action-Header
        (s/ordered-map
         :static/type   :notification
         :tag           (s/string :utf-8))))

(def Action-Give-Attribute-Point
  (into Action-Header
        (s/ordered-map
         :static/type   :give-attribute-point
         :amount        :int32)))

(def Action-Script-Event
  (into Action-Header
        (s/ordered-map
         :static/type   :script-event
         :event-name    (s/string :utf-8))))

(def Action-Give-Random-Item
  (into Action-Header
        (s/ordered-map
         :static/type   :give-random-item
         :dbr           (s/string :utf-8)
         :amount        :int32)))

(def Action-Generate-Random-Value
  (into Action-Header
        (s/ordered-map
         :static/type   :generate-random-value
         :name          (s/string :utf-8)
         :min           :int32
         :max           :int32)))

(def Action-Cast-Skill
  (into Action-Header
        (s/ordered-map
         :static/type   :cast-skill
         :dbr           (s/string :utf-8))))

(def Action-Unlock-Faction
  (into Action-Header
        (s/ordered-map
         :static/type   :unlock-faction
         :faction       (s/string :utf-8))))

(def Action-Set-Faction
  (into Action-Header
        (s/ordered-map
         :static/type   :set-faction
         :faction       (s/string :utf-8)
         :amount        :int32)))

(def Action-Give-Devotion
  (into Action-Header
        (s/ordered-map
         :static/type   :give-devotion
         :amount        :int32)))

(def Action-Give-Tribute
  (into Action-Header
        (s/ordered-map
         :static/type   :give-tribute
         :amount        :int32)))

(def Action-Unlock-Tutorial
  (into Action-Header
        (s/ordered-map
         :static/type   :unlock-tutorial
         :page          :int32)))

(def Action-Play-Video
  (into Action-Header
        (s/ordered-map
         :static/type   :play-video
         :path          (s/string :utf-8)
         :allow-skip    :bool)))

(def action-read-map
  (->> [Action-Begin-Quest
        Action-Begin-Quest-Task
        Action-Complete-Quest
        Action-Complete-Quest-Task
        Action-Give-Money
        Action-Give-Faction
        Action-Give-Item
        Action-Give-Level
        Action-Give-Experience
        Action-Give-Token
        Action-Remove-Token
        Action-Lua-Script
        Action-Debug-Print
        Action-Play-Sound
        Action-Play-Animation
        Action-Give-Skill-Point
        Action-Open-Merchant-Window
        Action-Notification
        Action-Give-Attribute-Point
        Action-Script-Event
        Action-Give-Random-Item
        Action-Generate-Random-Value
        Action-Cast-Skill
        Action-Unlock-Faction
        Action-Set-Faction
        Action-Give-Devotion
        Action-Give-Tribute
        Action-Unlock-Tutorial
        Action-Play-Video]
       (map-indexed vector)
       (into {})))

(defn read-action
  "Reads a single Action
  See ScriptableAction"
  [bb context]

  (let [type (.getInt bb)
        spec (action-read-map type)]

    (assert spec (u/fmt "Action spec type #{type} is not valid"))
    (s/read-struct spec bb context {:type type})))

(def QuestEvent
  (s/ordered-map
   :static/type  :quest-event
   :flags        :int32
   :conditions   read-conditions
   :actions      (s/variable-count read-action)))

(def QuestObjective
  (s/ordered-map
   :static/type  :quest-objective
   :uid          :int32
   :flags        :int32
   :conditions   read-conditions
   :actions      (s/variable-count read-action)))

(def QuestTask
  (s/ordered-map
   :static/type    :quest-task
   :uid            :int32
   :flags          :int32
   :on-accept      (s/variable-count QuestEvent)
   :objectives     (s/variable-count QuestObjective)
   :on-complete    (s/variable-count QuestEvent)
   :is-blocker     :bool
   :dont-propegate :bool))

(def Quest
  (s/ordered-map
   :static/type :quest
   :uid         :int32
   :flags       :int32
   :tasks       (s/variable-count QuestTask)))

(def StringTable
  (s/ordered-map
   :name      (s/string :utf-8)
   :tags      (s/variable-count (s/string :utf-8))))

(def StringTables
  (s/ordered-map
   :string-tables (s/variable-count StringTable)))

(defn read-block-start
  [bb context]

  (let [length (.getInt bb)
        expected-end-position (+ (.position bb) length)]
    {:length length
     :expected-end-position expected-end-position}))

(defn read-and-verify-block-end
  [bb context id expected-end-position]

  ;; Verify we've reached the expected position
  (assert (= expected-end-position (.position bb))
          (utils/fmt "[Block #{id}]Expected to be at stream position: #{expected-end-position}, but current at #{(.position bb)}. Position is off by: #{(- expected-end-position (.position bb) )}"))

  ;; Verify we have the correct enc-state at this point
  (let [checksum (Integer/toUnsignedLong (.getInt bb))]
    (assert (= checksum (:enc-state @context)))))

(defn- validate-preamble
  [preamble]

  (when (or (not= (:magic preamble) (u/file-magic "QST2")))
    (throw (Throwable. "I don't understand this gdd format!"))))


(defn- collect-string-bind-targets
  [quest-file]
  (u/collect-walk #(contains? #{:quest
                                :quest-event
                                :quest-objective
                                :quest-task}
                              (:static/type %))
                  (:quest quest-file)))

(defn- get-string-table
  [quest-file locale-name]

  (some #(when (= locale-name (:name %))
           %)
        (:string-tables quest-file)))

(defn quest-bind-strings
  [quest-file locale-name]

  ;; Locate the string-table we're trying to bind/merge
  (if-let [string-table (get-string-table quest-file locale-name)]

    ;; Locate all the items in the quest tree we want to bind text to
    (let [bind-targets (collect-string-bind-targets quest-file)]

      ;; Walk through the each of the targets
      ;; Insert the strings found in the table
      ;; Accumulate results into `quest-file`
      (loop [target-list bind-targets
             table (drop 1 (:tags string-table))
             quest-file quest-file]

        (let [target (first target-list)]
          (cond
            (nil? target) quest-file  ;; No more items to process? Return the accumulated results

            (= (:static/type (second target)) :quest-task)
            (recur (rest target-list)
                   (drop 2 table)
                   (-> quest-file
                       ;; Assoc the first item in the table into the item as :text
                       (update-in (apply conj [:quest] (first target))
                                  assoc :text (first table))

                       ;; Assoc the second item in the table into the item as :description
                       (update-in (apply conj [:quest] (first target))
                                  assoc :description (second table)))
                   )

            :else
            (recur (rest target-list)
                   (rest table)
                   (update-in quest-file
                              (apply conj [:quest] (first target))
                              assoc :text (first table)))))))))


(defn load-quest
  "Load a quest-file map from the given ByteBuffer"
  [^ByteBuffer bb]

  (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

  (let [context (atom {})

        preamble (s/read-struct FilePreamble bb)
        _ (validate-preamble preamble)

        block-header (read-block-start bb context)
        quest (s/read-struct Quest bb context)
        string-tables (s/read-struct StringTables bb context)]

    (assert (zero? (.remaining bb))
            (u/fmt "Should have reached the end of file, but still have #{(.remaining bb)} bytes remaining"))

    (-> {:quest quest}
        (merge string-tables)
        (quest-bind-strings "enUS"))
    ))


(defn load-quest-file
  "Loads a quest-file map from the given file path"
  [filepath]

  (load-quest (utils/file-contents filepath)))

(defn quest-name
  "Get the overall name of the quest"
  [quest]

  (:text quest))

(defn locate-quest-tokens
  [quests]

  (u/collect-walk #(and (= :give-token
                           (:static/type %)))
                  quests))

(defn locate-quest-uids
  [quests]
  (u/collect-walk #(and (map? %)
                        (contains? % :uid))
                  quests))

(defn find-quest-for-token
  [quests token]

  (u/collect-walk #(and (contains? #{:give-token}

                           (:static/type %)))
                  quests))

(defn get-quest-by-uid
  [quest-uid-index uid]

  (->> (quest-uid-index uid)
              (first)
              (second)))

(defn quest-item-attach-name
  [quest-item-uid-index quest-item]

  (assoc quest-item
         :name
         (->> (or (:id1 quest-item) (:uid quest-item))
              (get-quest-by-uid quest-item-uid-index)
              (:text))))

(comment

  (def quest-progress
    gd-edit.gdd-reader/quest-progress)

  (:tokens quest-progress)
  (->> quest-progress
       (:quests)
       (drop 1)
       (first))

  (def t
    (gd-edit.arc-reader/load-arc-file "/Volumes/Untitled/Program Files (x86)/Steam/steamapps/common/Grim Dawn/resources/Quests.arc")
    )

  (first t)


  ;; Load all records in a arc file
  (def quests
    (->> t
         (map (fn [{:keys [recordname contents]}] (merge {:recordname recordname}
                                                        (->> contents
                                                             (ByteBuffer/wrap)
                                                             (load-quest)
                                                             (:quest))
                                                        )))))

  (def z
    (u/collect-walk #(= :give-token (:static/type %)) s))


  (->> s
       (map quest-name))

  (find-quest-for-token quests "SALT_NECKLACE")

  (->> quests
       (filter #(clojure.string/includes? (:recordname %) "sq_slithneck")))


  ;; Index of tokens given as a part of some quest
  ;; This doesn't include all tokens which may be given as part of an interaction with NPCs
  ;; Those are found in lua script files
  (group-by #(:token (second %))
            (locate-quest-tokens quests))


  (->> quest-progress
       (:quests)
       (first))

  (first quests)


  ;; Quest group uid index
  (def quest-item-uid-index
    (->> (locate-quest-uids quests)
         (group-by #(:uid (second %)))))


  (specter/select [:quests specter/ALL :tasks specter/ALL] quest-progress)

  ;; Annotate the quest progress with readable names
  (->> (:quests quest-progress)

       ;; Attach names to all tasks
       (specter/transform [specter/ALL :tasks specter/ALL]
                          #(quest-item-attach-name quest-item-uid-index %))

       ;; Attach names to all quests
       (map #(quest-item-attach-name quest-item-uid-index %)))


  (quest-item-attach-name
   quest-item-uid-index
   (->> (:quests quest-progress)
        (last)
        (:tasks)
        (first)))

  )
