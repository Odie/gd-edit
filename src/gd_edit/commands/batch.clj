(ns gd-edit.commands.batch
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [jansi-clj.core :refer [green red yellow]]
            [gd-edit.commands.choose-character :as choose-character]
            [gd-edit.commands.write :as write]
            [gd-edit.utils :as u]
            [gd-edit.progress-bar :as progress]
            )
  (:import [sun.misc Signal SignalHandler]))

(defn batch-handler
  [[_ tokens]]

  (let [batch-file (io/file (first tokens))
        batch-repl (find-var 'gd-edit.core/batch-repl)]
    (if (not (.exists batch-file))
      (u/print-line (format "%s:\n\t%s" (red "Cannot locate the batch file") (yellow (.getCanonicalPath batch-file))))

      (do
        (batch-repl
         (->> (io/file batch-file)
              (slurp)
              (str/split-lines)))
        (u/print-line)
        (u/print-line (green "Batch file completed!"))))))

(defn setup-INT-handler
  [context-a]

  ;; Sanity check
  ;; Do not setup the handler multiple times
  (when-not (:cur-handler @context-a)
    (let [;; Create a new handler for the INT signal
          new-handler (proxy [SignalHandler] []
                        (handle [sig]
                          ;; When the signal is detected...
                          ;; Set a flag in the context atom
                          ;; We're going to pick up the signal elsewhere
                          ;; in the code
                          (swap! context-a assoc :INT-detected true)
                          (println "\nStopping batch character...")))

          ;; Hook in the new handler
          ;; Store away the old handler so it can be restored later
          old-handler (Signal/handle (Signal. "INT") new-handler)]

      ;; Store both handlers to be referenced later
      (reset! context-a
              {:cur-handler new-handler
               :old-handler old-handler}))))

(defn teardown-INT-handler
  [context-a]
  (when (:old-handler @context-a)
    ;; Restore the old signal handler
    (Signal/handle (Signal. "INT") (:old-handler @context-a))))

(defn batch-character-handler
  [[_ tokens]]

  (let [batch-file (io/file (first tokens))
        verbose? (u/case-insensitive= (second tokens) "verbose")
        batch-repl (find-var 'gd-edit.core/batch-repl)]
    (if (not (.exists batch-file))
      (u/print-line (format "%s:\n\t%s" (red "Cannot locate the batch file") (yellow (.getCanonicalPath batch-file))))

      (let [batch-cmds (->> (io/file batch-file)
                            (slurp)
                            (str/split-lines))
            char-locs (choose-character/character-list)
            handler-context (atom {})
            _ (setup-INT-handler handler-context)

            completion-count (atom 0)

            start-time (System/nanoTime)

            char-count (count char-locs)]

        ;; Optionally turn off all prints while executing the batch file
        (binding [u/*suppress-print* (not verbose?)]
          ;; Loop over each of the known characters
          (loop [char-list (u/indexed char-locs)]
            (cond
              ;; Stop when the list is exhausted or...
              ;; when the user wants to cancel/stop the batch process
              (not char-list) :ok
              (:INT-detected @handler-context) :interrupted

              :else
              (let [[idx char-loc] (first char-list)]
                ;; Optionally show the progress bar
                (when (not verbose?)
                  (progress/display-progress (inc idx) char-count))

                ;; Load up the character file
                (choose-character/load-character-file (:gdc-path char-loc))

                ;; Run the batch commands
                (batch-repl batch-cmds)

                ;; Write out the character file
                (write/write-loaded-character-file!)
                (u/print-line)
                (swap! completion-count inc)

                ;; Proceed with the next character
                (recur (next char-list))))))

        (teardown-INT-handler handler-context)

        (u/newline-)
        (u/print-line (format (green "Batch file completed for %d characters!") @completion-count))
        (u/print-line (format (green "Elapsed: %f seconds") (u/nanotime->secs (- (System/nanoTime) start-time))))))))
