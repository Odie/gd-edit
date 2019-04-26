(ns gd-edit.db-query
  (:require [clj-fuzzy.metrics :as metrics]
            [clojure.string :as str]
            [gd-edit.utils :as u]))

(defn pair-has-key
  [name]

  ;; given a a pair of items, where the first item is presumably the key
  ;; of some hashmap
  ;; Answer if the key contains a string
  (fn [pair]
    (-> (first pair)
        (str/lower-case)
        (.contains (str/lower-case name)))))

(defn hashmap-has-key
  [name]

  (fn [map]
    (->> (keys map)
         (some (fn [key]
                 (-> (str/lower-case key)
                     (.contains name)))))))

(defmacro qand
  [& xs]

  `(fn [[~'key ~'value]]
    (and ~@xs)))

(defmacro qpred
  [& xs]

  `(fn [[~'key ~'value]]
     ~@xs))

(defn- predicates->result
  "Given a db and some predicates, return a list of results that satisfies all predicates."
  [db predicates]

  (reduce
   (fn [accum query-pred]

     ;; We're walking over the query predicates
     ;; Each time, we'll narrow the accum result further by the current
     ;; query item
     (filter (fn [kv-map]
               (some (fn [kv-pair]
                       ;; Is the query predicate actually another sequence of predicates?
                       (if (not (sequential? query-pred))

                         ;; If not, we assume this is a single predicate
                         ;; Execute it as is
                         (query-pred kv-pair)

                         ;; Otherwise, we have a sequence of predicates that this kv-pair
                         ;; must all satisfy
                         ;; This means we can construct a series of simple predicates
                         ;; and make sure it's checking on the same kv-pair
                         (every? (fn [pred] (pred kv-pair))
                                 query-pred)))
                     kv-map)
               )
             accum))

   db
   predicates))


(defn string-best-match
  [distance-fn haystack needle]

  (reduce
     (fn [best candidate]

       (if (> (distance-fn best needle)
              (distance-fn candidate needle))
         candidate
         best))

     (first haystack)
     haystack))

(defn- string-best-matches
  "Transform candidates into their best matches defined by the fieldnames"
  [fieldnames candidates]

  (map
   (fn [item]
     (string-best-match metrics/jaccard fieldnames item))

   candidates))


(defn- order-results
  "Order the results by the values of given in the order vector"
  [result order]

  (let [order-fields (string-best-matches (->> result
                                               first
                                               keys
                                               (map str))
                                          order)]
    (cond
      (not (empty? order-fields))
      (reduce
       (fn [transformed-result sort-field]
         (->> transformed-result
              (sort-by (fn [record] (or (get record sort-field) 0)) >)))
       result
       (reverse order-fields))

      ;; By default, we'll sort the results by the recordname
      :else
      (->> result
           (sort-by :recordname)))))

(defn query
  "Given a db (list of maps) and some predicates, return all maps that
  satisfies all predicates "
  [db {:keys [predicates order]}]

  (-> (predicates->result db predicates)
      (order-results order)))

(defn query-string-compare
  [op record-val query-val]

  ;; Strings can only be compared using ~ = !=
  ;; All other ops will return false
  (cond
    (or (= op "~") (= op "*="))
    (u/ci-match record-val query-val)

    (= op "=")
    (= record-val query-val)

    (= op "!=")
    (not= record-val query-val)

    :else
    false))

(defn query-numeric-compare
  [op record-val query-val]

  (cond
    (= op "=")
    (= record-val query-val)

    (= op ">")
    (> record-val query-val)

    (= op "<")
    (< record-val query-val)

    (= op "!=")
    (not= record-val query-val)

    (= op ">=")
    (>= record-val query-val)

    (= op "<=")
    (<= record-val query-val)

    :else
    false))


(defn- numeric?
  [item]

  (or (integer? item) (float? item)))


(defn query-compare
  [op record-val query-val]

  (cond
    (and (string? record-val) (string? query-val))
    (query-string-compare op record-val query-val)

    (and (numeric? record-val) (numeric? query-val))
    (query-numeric-compare op record-val query-val)

    :else
    false))

(defn strip-quotes
  [input]
  (str/replace input #"^\"|\"$" ""))

(defn coerce-query-val
  [input]

  (cond
    ;; Does it look like an integer?
    (re-matches #"^[0-9]*$" input)
    (Integer/parseInt input)

    (re-matches #"^[0-9]*(?:\.[0-9]*)?$" input)
    (Float/parseFloat input)

    :else
    (strip-quotes input)))

(defn valid-target?
  [target]

  (if (contains? #{"recordname" "key" "value"} target)
    true
    false))

(defn valid-op?
  [op]

  (if (contains? #{"~" "*=" "=" "!=" ">" "<" ">=" "<="} op)
    true
    false))

(defn tokens->query-predicate
  "Given a list of token that represents a query clause, return a predicate suitable for used
  by the query function"
  [[target op query-val & rest]]

  ;; (if-not (valid-target? target)
  ;;   (throw (Throwable. (format "\"%s\" is not a valid target" target))))
  (if (nil? op)
    (throw (Throwable. "No op supplied")))
  (if-not (valid-op? op)
    (throw (Throwable. (format "\"%s\" is not a valid op" op))))
  (if (nil? query-val)
    (throw (Throwable. "Missing query value")))

  (let [coerced-val (coerce-query-val query-val)]
    (cond

      (= target "key")
      (qpred (query-compare op key coerced-val))

      (= target "value")
      (qpred (query-compare op value coerced-val))

      ;; If the target doesn't look like a valid target,
      ;; we assume that the user is trying to target a key/value pair.
      ;; This case was originally used to handle "recordname ~ <some value>".
      ;; However, we can also use this to handle cases such as:
      ;;   "levelreq = 65"
      ;; This makes queries less verbose.
      :else
      (qand  (u/ci-match key target)
             (query-compare op value coerced-val)))))

;; The outputted query predicates is currently a list of predicates.
;; An item might also be a vector, in which case, a single kv-pair must satisfy all the predicates
(defn query-ast->query-predicates
  [query-ast]

  (loop [ast query-ast
         result {:order []
                 :predicates []}]

    (cond
      ;; Done walking through the ast?
      (empty? ast)
      result

      ;; order keyword found? sort the info into a separate :order vector
      (= (str/lower-case (first ast)) "order")
      (recur (drop 2 ast)
             (update result :order conj (second ast)))

      ;; Take a peek at the first item
      (not (sequential? (first ast)))
      ;; If it's not a vector, assume we can just grab 3 items and use that as a clause
      ;; Note that we're not checking any kind of grammar here...
      (recur (drop 3 ast)
             (update result :predicates conj (tokens->query-predicate (take 3 ast))))

      ;; If it is a vector, we're going to recursively process that node and append the results
      (sequential? (first ast))
      (recur (drop 1 ast)
             (update result :predicates conj (:predicates (query-ast->query-predicates (first ast)))))
      )))

(defn tokens->query-ast
  "Given a list of token that represents a query, return a sequence that can be used to run the
  query."
  [tokens]

  ;; Walk through the tokens and place any sub-expressions a expression list
  (loop [ts tokens
         ast-stack [[]]]

    (let [token (first ts)]
      (cond
        ;; Did we exhaust the input tokens?
        (nil? token)
        (do
          ;; We should be left with a single constructed ast node
          ;; If this didn't happen, then there is an un-closed paren in the query string somewhere
          (if (not= (count ast-stack) 1)
            (throw (Throwable. "unmatched parentheses")))
          (first ast-stack))

        ;; Opening new expression
        (= token "(")
        (recur (rest ts)
               (conj ast-stack [])) ;; push the new expression onto the stack

        ;; or closing the current expression?
        ;; Move whatever we've collected into the last item of the ast
        ;; Grab the last 2 items, conj the last one into the second to the last one
        (= token ")")
        (let [last-node (peek ast-stack)
              ast (pop ast-stack)
              ]
          (recur (rest ts)
                 (assoc ast
                        (dec (count ast))
                        (conj (last ast) last-node))))

        ;; Just another token?
        :else
        (recur (rest ts)
               ;; Drop the item in as the last item on the top of the stack
               (assoc ast-stack
                      (dec (count ast-stack))
                      (conj (last ast-stack) token)))
        ))))

(defn query-string->tokens
  [input]
  (into [] (re-seq #"(?:\!\=)|(?:\>\=)|(?:\<\=)|(?:\*\=)|[\(\)\~\<\>\=]|\"[^\"]+\"|[\w\/\.]+" input)))

(defn query-string->query-predicates
  [input]

  (-> input
      (query-string->tokens)
      (tokens->query-ast)
      (query-ast->query-predicates)))

(defn query-db
  "Help function to execute a quick query against the db.
  Although it is possible to manually run a reduce over the db, we have to at least
  duplicate *some* of the code in this module to make it happen.
  This module's job is to make querying the db simple, so we might as well use it interally
  when appropriate."
  [db query-string]

  (query db (query-string->query-predicates query-string)))
