(ns gd-edit.command-handlers
  (:require [gd-edit.db-query :as query]
            [gd-edit.globals :as g]
            [gd-edit.utils :as u]
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
  [result {:keys [page pagination-size] :as query-state}]

  (->> result
       (drop (* page pagination-size))
       (take pagination-size)))


(defn set-new-query-result!
  [query-state-atom new-result query-string]

  (reset! g/query-state (assoc @g/query-state
                               :query-string query-string
                               :result new-result
                               :page 0)))


(defn print-result-records
  [results]
  (doseq [kv-map results]
    (println (:recordname kv-map))
    (doseq [[key value] kv-map]

      (when (not= key :recordname)
        (println (format "\t%s: %s" (style key :white) (style value :darkgray)))
        ))

    (println)
    ))


(defn- order-query-results
  [result query-state]

  (let [{:keys [filter-max-fields]} query-state]

    ;; How will we display the results?
    (cond
      (not (empty? filter-max-fields))
      (reduce
       (fn [transformed-result sort-field]
         (->> transformed-result
              (sort-by (fn [record] (get record sort-field)) >)))
       result
       filter-max-fields
       )

      ;; By default, we'll sort the results by the recordname
      :else
      (->> result
           (sort-by :recordname))
      )))

(defn print-paginated-result
  [query-state]

  (let [{:keys [result page pagination-size]} query-state
        paginated-result (get-paginated-result result query-state)
        start-entry (* page pagination-size)
        end-entry (+ start-entry (min (count paginated-result) pagination-size))
        ]

    (print-result-records paginated-result)
    (println)
    (println (format "%d-%d / %d %s" start-entry end-entry (count result) (style "matched records" :darkgray)))))


(defn query-show-handler
  [[input tokens]]

  (paginate-next! g/query-state)
  (print-paginated-result @g/query-state))


(defn run-query
  [input]

  (let [predicates (try (query/query-string->query-predicates input)
                        (catch Throwable e (println (str "Query syntax error: " (.getMessage e)))))]

    (when (not (nil? predicates))

      ;; Run a query against the db using generated predicates
      (set-new-query-result! g/query-state
                             (-> (apply query/query @g/db predicates)
                                 (order-query-results @g/query-state))
                             input)

      (print-paginated-result @g/query-state))))

(defn query-comand-handler
  [[input tokens]]

  (if (empty? input)
      (println
"usage: q <target> <op> <value>
       <target> can be \"recordname\", \"value\", or \"key\"")
      (run-query input)))


(defn query-filter-handler
  [[input tokens]]

  (if (and (u/ci-match (first tokens) "max") (not (nil? (second tokens))))
    (swap! g/query-state update :filter-max-fields conj (second tokens)))

  (println "Maximizing by fields: " (:filter-max-fields @g/query-state)))
