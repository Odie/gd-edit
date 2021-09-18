(ns gd-edit.commands.batch
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [jansi-clj.core :refer [green red yellow]]
            [gd-edit.commands.choose-character :as choose-character]
            [gd-edit.commands.write :as write]
            [gd-edit.utils :as u]))

(defn batch-handler
  [[_ tokens]]

  (let [batch-file (io/file (first tokens))
        batch-repl (find-var 'gd-edit.core/batch-repl)]
    (if (not (.exists batch-file))
      (println (format "%s:\n\t%s" (red "Cannot locate the batch file") (yellow (.getCanonicalPath batch-file))))

      (do
        (batch-repl
         (->> (io/file batch-file)
              (slurp)
              (str/split-lines)))
        (println)
        (println (green "Batch file completed!"))))))

(defn batch-character-handler
  [[_ tokens]]

  (let [batch-file (io/file (first tokens))
        batch-repl (find-var 'gd-edit.core/batch-repl)]
    (if (not (.exists batch-file))
      (println (format "%s:\n\t%s" (red "Cannot locate the batch file") (yellow (.getCanonicalPath batch-file))))

      (let [batch-cmds (->> (io/file batch-file)
                            (slurp)
                            (str/split-lines))
            char-locs (choose-character/character-list)
            start-time (System/nanoTime)]
        (doseq [char-loc char-locs]
          (choose-character/load-character-file (:gdc-path char-loc))
          (batch-repl batch-cmds)
          (write/write-loaded-character-file!)
          (println))

        (println (format (green "Batch file completed for %d characters!") (count char-locs)))
        (println (format (green "Elapsed: %f seconds") (u/nanotime->secs (- (System/nanoTime) start-time))))))))
