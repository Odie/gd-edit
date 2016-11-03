(ns gd-edit.db-query
  (:require [clojure.string :as string]))


(defn case-insensitive-match
  "Check if str2 can be found in str1"
  [str1 str2]

  (.contains (string/lower-case (str str1)) (string/lower-case (str str2))))

(def ci-match case-insensitive-match)

(defn pair-has-key
  [name]

  ;; given a a pair of items, where the first item is presumably the key
  ;; of some hashmap
  ;; Answer if the key contains a string
  (fn [pair]
    (-> (first pair)
        (string/lower-case)
        (.contains (string/lower-case name)))))

(defn hashmap-has-key
  [name]

  (fn [map]
    (->> (keys map)
         (some (fn [key]
                 (-> (string/lower-case key)
                     (.contains name)))))))

(defmacro qand
  [& xs]

  `(fn [[~'key ~'value]]
    (and ~@xs)))

(defmacro qpred
  [& xs]

  `(fn [[~'key ~'value]]
     ~@xs))

(defn query
  "Given a db (list of maps) and some predicates, return all maps that
  satisfies all predicates "
  [db & predicates]

  (reduce
   (fn [accum query-pred]

     ;; We're walking over the query predicates
     ;; Each time, we'll narrow the accum result further by the current
     ;; query item
     (filter (fn [kv-map]
               (some (fn [kv-pair] (query-pred kv-pair))
                     kv-map)
               )
             accum))

   db
   predicates))

(defn token->op-fn
  [op-token]

  (cond
    (= op-token "~")
    ci-match

    (= op-token "=")
    =

    (= op-token ">")
    >

    (= op-token "<")
    <

    (= op-token "!=")
    not=))

(defn tokens->query-predicate
  [[target op query-val & rest]]

  (cond
    (= target "key")
    (qpred ((token->op-fn op) key query-val))

    (= target "value")
    (qpred ((token->op-fn op) value query-val))

    :else
    (qand  (ci-match key target)
           ((token->op-fn op) value query-val))))

#_(def r (time
          (query gd-edit.core/db
                 (tokens->query-predicate ["recordname" "~" "affix"])
                 (tokens->query-predicate ["key" "~" "cold"])
                 (tokens->query-predicate ["levelreq" "=" 74])
                 )))
