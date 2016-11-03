(ns gd-edit.command-handlers
  (:require [gd-edit.db-query :as query]
            [gd-edit.globals :as globals]))

(defn query-show-comand-handler
  []

  )

(defn print-result-records
  [results]
  (doseq [kv-map results]
    (println (:recordname kv-map))
    (doseq [[key value] kv-map]

      (when (not= key :recordname)
        (println "\t" key ":" value)
        ))

    (println)
    ))

(defn query-comand-handler
  [[input tokens]]

  ;; TODO Add token validation!

  (let [clauses (partition 3 tokens)
        predicates (map query/tokens->query-predicate clauses)
        results (apply query/query @globals/db predicates)
        results-for-display (->> results
                                (sort-by :recordname)
                                (take 10))
        ]

    (print-result-records results-for-display)

    (println)
    (println (count results-for-display) "/" (count results) "matched records")))
