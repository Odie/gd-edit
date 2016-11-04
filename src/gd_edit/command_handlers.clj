(ns gd-edit.command-handlers
  (:require [gd-edit.db-query :as query]
            [gd-edit.globals :as g]
            [clansi.core :refer [style]]))

(defn paginate-next
  [page {:keys [pagination-size] :as query-state}]

  ;; How many results do we have?
  (let [size (count (:result query-state))

        ;; How many items have we shown?
        shown-so-far (* (inc (or page 0)) pagination-size)]

    ;; If we haven't shown everything yet...
    (if (< shown-so-far size)
      ;; Show the next page
      (inc page)

      ;; If we've already shown everything...
      ;; Go back to the beginning
      0
      )))

(defn paginate-next!
  [query-state-atom]
  (swap! query-state-atom update :page (fn [oldval] (paginate-next oldval @query-state-atom))))


(defn get-paginated-result
  [{:keys [result page pagination-size] :as query-state}]

  (->> result
       (drop (* page pagination-size))
       (take pagination-size)))


(defn set-new-query-result!
  [query-state-atom new-result]

  (reset! g/query-state (assoc @g/query-state
                               :result new-result
                               :page 0)))


(defn print-result-records
  [results]
  (doseq [kv-map results]
    (println (:recordname kv-map))
    (doseq [[key value] kv-map]

      (when (not= key :recordname)
        (println (format "\t%s: %s" key value))
        ))

    (println)
    ))

(defn print-paginated-result
  [query-state]

  (let [{:keys [result page pagination-size]} query-state
        paginated-result (get-paginated-result query-state)
        start-entry (* page pagination-size)
        end-entry (+ start-entry (min (count paginated-result) pagination-size))
        ]

    (print-result-records paginated-result)
    (println)
    (println (format "%d-%d / %d matched records" start-entry end-entry (count result)))))


(defn query-show-handler
  [[input tokens]]

  (paginate-next! g/query-state)
  (print-paginated-result @g/query-state))


(defn query-comand-handler
  [[input tokens]]

  ;; TODO Add token validation!

  (let [;; Generate predicates from the input tokens
        clauses (partition 3 tokens)
        predicates (map query/tokens->query-predicate clauses)

        ;; Run a query against the db using generated predicates
        result (->> (apply query/query @g/db predicates)
                     (sort-by :recordname))]

    (set-new-query-result! g/query-state result)
    (print-paginated-result @g/query-state)))
