(ns gd-edit.commands.batch
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [jansi-clj.core :refer [green]]))

(defn batch-handler
  [[_ tokens]]

  (let [batch-file (io/file (first tokens))
        batch-repl (find-var 'gd-edit.core/batch-repl)]
    (if (not (.exists batch-file))
      (println "Cannot locate the batch file")

      (do
        (batch-repl
         (->> (io/file batch-file)
              (slurp)
              (str/split-lines)))
        (println)
        (println (green "Batch file completed!"))))))
