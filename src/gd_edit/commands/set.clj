(ns gd-edit.commands.set
  (:require [gd-edit.app-util :as au]
            [gd-edit.commands
             [choose-character :as commands.choose-character]
             [item :as commands.item]]
            [clojure.string :as str]
            [gd-edit.db-utils :as dbu]
            [gd-edit.structure-walk :as sw]
            [gd-edit.globals :as globals]
            [gd-edit.utils :as u]
            [gd-edit.printer :as printer]
            [jansi-clj.core :refer [red green yellow]]))

(defn- find-item-component-by-name
  [db component-name]

  (->> db
       (filter (fn [record]
                 (and
                  (u/case-insensitive-match (:recordname record) "items/materia")
                  (u/case-insensitive-match (record "description") component-name))))
       (first)))

(defn- set-item--relic-name
  [db update-path newval]

  ;; An item can have a relic/component attached
  ;; To make the program a bit more friendly to use, the user can set the relic-name to a
  ;; the name of a record, or just the English display name of a relic/component.
  (let [component-record (find-item-component-by-name db newval)]
        (cond

          ;; Does the component name look like a display name of a component?
          component-record
          (reset! globals/character
                  (update-in @globals/character update-path (constantly (:recordname component-record))))


          ;; Otherwise, we just set
          :else
          (swap! globals/character update-in update-path (constantly newval)))))

(defn set-character-field
  [character path newval]

  (let [;; What's the current value at that location?
        value (get-in character path)

        ;; What is the type of the value? We'll have to coerce the supplied new value
        ;; to the same type first.
        newval (try (dbu/coerce-to-type newval (type value))
                    (catch Exception _ :failed))]

    ;; Set the new value into the character sheet
    (update-in character path (constantly newval))))

(defn path-sibling
  "Given a path as a vector, return a path to the 'sibling'."
  [path sibling-field]

  (conj (->> path
             (butlast)
             (into []))
        sibling-field))

(defn- warn-user-before-set-val
  "Warns the user if the changing a field might be problematic.
  Returns false if the user chooses to abort the value change. Otherwise, returns true."
  [character field-path new-val]

  (cond
    (and (= 1 (count field-path))
         (contains? #{:level-in-bio :max-level :character-level} (first field-path)))
    (printer/prompt-yes-no ["Changing the level manually is not recommended."
                            "Please use the 'level' command instead."
                            (u/fmt "Try 'level #{new-val}'.")
                            ""
                            "Are you sure you want to change the value of this field manually?"]
                           false)
    :else true))

(defn set-handler
  [[input tokens]]

  (cond
    ;; If the character hasn't been loaded...
    ;; Move to the character selection screen first
    (not (au/character-loaded?))
    (commands.choose-character/character-selection-screen!)

    (< (count tokens) 2)
    (println "Usage: show <path> <new-value>")

    ;; Split a path into components.
    ;; We're going to use these as keys to navigate into the character sheet
    :else
    (let [path-keys (str/split (first tokens) #"/")
          walk-result (sw/walk @globals/character path-keys)
          {:keys [status]} walk-result]

      (cond
        (= status :not-found)
        (println "No matches found")

        (= status :too-many-matches)
        (sw/print-ambiguous-walk-result walk-result)

        ;; We found the value the user wanted to update
        (= status :found)
        (let [value (:found-item walk-result)
              ;; What is the type of the value? We'll have to coerce the supplied new value
              ;; to the same type first.
              newval (try (dbu/coerce-to-type (second tokens) (type value))
                          (catch Exception _ :failed))
              val-path (:actual-path walk-result)]

          (cond
            ;; Is the targetted value an item?
            ;; Let the set-item handler deal with it
            (dbu/is-item? value)
            (commands.item/set-item-handler [input tokens])

            ;; Did the user specify the some inventory items collection?
            (and (= (first val-path) :inventory-sacks)
                 (= (last val-path) :inventory-items))
            (commands.item/set-item-handler [input tokens])


            (and
             (dbu/is-item? (get-in @globals/character (butlast val-path)))
             (= :relic-name (last val-path)))
            (do
              (set-item--relic-name (dbu/db) val-path newval)
              (swap! globals/character
                     set-character-field
                     ;; Set the item's relic-completion-level field...
                     (path-sibling val-path
                                   :relic-completion-level)

                     ;; to 4
                     4))


            ;; The user cannot create a collection directly from the commandline.
            ;; So replacing a collection directly makes no sense and cannot be done.
            (coll? value)
            (println "Sorry, can't set the value of" (first tokens))

            ;; If coercion failed, tell the user what type is being expected
            (= newval :failed)
            (println "Please provide a value that is of type" (-> (type value)
                                                                  (str)
                                                                  (str/split #"\.")
                                                                  (last)
                                                                  (str/lower-case)))

            :else
            (if-not (warn-user-before-set-val @globals/character val-path newval)
              (println (red"Command aborted."))
              ;; Set the new value into the character sheet
              (reset! globals/character
                      (update-in @globals/character val-path (constantly newval)))))
          nil)

        :else
        (throw (Throwable. "Unhandled case"))))))
