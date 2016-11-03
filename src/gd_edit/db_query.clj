(ns gd-edit.db-query
  (:require [clojure.string :as string]))

;; (defn partial-match
;;   [name]

;;   (fn [item]
;;     (-> item
;;         (string/lower-case)
;;         (.contains name)
;;         )))

(defn case-insensitive-match
  "Check if str2 can be found in str1"
  [str1 str2]

  (.contains (string/lower-case (str str1)) (string/lower-case (str str2))))

(defn pair-has-key
  [name]

  ;; given a a pair of items, where the first item is presumably the key
  ;; of some hashmap
  ;; Answer if the key contains a string
  (fn [pair]
    (-> (first pair)
        (string/lower-case)
        (.contains (string/lower-case name))
        )))

(defn hashmap-has-key
  [name]

  (fn [map]
    (->> (keys map)
         (some (fn [key]
                 (-> (string/lower-case key)
                     (.contains name))
                 ))
         )))

(defmacro pred-and
  [& xs]

  `(fn [[~'key ~'value]]
    (and ~@xs)))

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


(def r (time
        (query gd-edit.core/db
               (fn [[key value]]
                 (and  (case-insensitive-match key "recordname")
                       (case-insensitive-match value "affix")))

               (fn [[key value]]
                 (case-insensitive-match key "cold"))

               (fn [[key value]]
                 (case-insensitive-match key "levelreq")
                 (= value 74))
               )))
