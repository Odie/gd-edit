(ns gd-edit.commands.batch
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [jansi-clj.core :refer [green red yellow]]
            [gd-edit.commands.choose-character :as choose-character]
            [gd-edit.commands.write :as write]
            [gd-edit.utils :as u]
            [gd-edit.progress-bar :as progress]
            ))

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

(defn batch-character-handler
  [[_ tokens]]

  (let [batch-file (io/file (first tokens))
        batch-repl (find-var 'gd-edit.core/batch-repl)]
    (if (not (.exists batch-file))
      (u/print-line (format "%s:\n\t%s" (red "Cannot locate the batch file") (yellow (.getCanonicalPath batch-file))))

      (let [batch-cmds (->> (io/file batch-file)
                            (slurp)
                            (str/split-lines))
            char-locs (choose-character/character-list)
            start-time (System/nanoTime)
            char-count (count char-locs)]
        (binding [u/*suppress-print* true]
          (doseq [[idx char-loc] (u/indexed char-locs)]
          ;; (doseq [char-loc char-locs]
            ;; (progress/display-file-progress (inc idx) char-count)
            (progress/display-progress (inc idx) char-count)
            (print "\r")
            ;; (print ".")
            (choose-character/load-character-file (:gdc-path char-loc))
            (batch-repl batch-cmds)
            (write/write-loaded-character-file!)
            (u/print-line)))

        (u/newline-)
        (u/print-line (format (green "Batch file completed for %d characters!") (count char-locs)))
        (u/print-line (format (green "Elapsed: %f seconds") (u/nanotime->secs (- (System/nanoTime) start-time))))))))
