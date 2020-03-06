(ns gd-edit.commands.query
  (:require [gd-edit.globals :as globals]
            [gd-edit.printer :as printer]
            [jansi-clj.core :as jansi :refer [red green yellow]]
            [gd-edit.db-query :as query]
            [gd-edit.db-utils :as dbu]))

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
      0)))

(defn paginate-next!
  [query-state-atom]
  (swap! query-state-atom update :page (fn [oldval] (paginate-next oldval @query-state-atom))))

(defn get-pagination-size
  [result {:keys [pagination-size] :as query-state}]
  (if (string? (first result))
    100
    pagination-size))

(defn get-paginated-result
  [result {:keys [page] :as query-state}]

  (let [pagination-size (get-pagination-size result query-state)]
    (->> result
         (drop (* page pagination-size))
         (take pagination-size))))
(defn set-new-query-result!
  [query-state-atom new-result query-string]

  (reset! globals/query-state (assoc @globals/query-state
                                     :query-string query-string
                                     :result new-result
                                     :page 0)))

(defn print-paginated-result
  [query-state]

  (let [{:keys [result page]} query-state
        pagination-size (get-pagination-size result query-state)
        paginated-result (get-paginated-result result query-state)
        start-entry (* page pagination-size)
        end-entry (+ start-entry (min (count paginated-result) pagination-size))]

    (printer/print-result-records paginated-result)
    (println)
    (println (format "%d-%d / %d %s" start-entry end-entry (count result) (yellow "matched records")))))

(defn query-show-handler
  [[input tokens]]

  (paginate-next! globals/query-state)
  (print-paginated-result @globals/query-state))

(defn run-query
  [input]

  (let [predicates (try (query/query-string->query-predicates input)
                        (catch Throwable e (println (str "Query syntax error: " (.getMessage e)))))]

    (when (not (nil? predicates))
      ;; Run a query against the db using generated predicates
      (set-new-query-result! globals/query-state
                             (query/query (dbu/db) predicates)
                             input)

      (print-paginated-result @globals/query-state))))

(defn query-command-handler
  [[input tokens]]

  (cond
    (empty? input)
    (println
     "usage: q <target> <op> <value>
       <target> can be \"recordname\", \"value\", or \"key\"")

    (not (realized? globals/db))
    (println "db not yet loaded")

    :else
    (run-query input)))
