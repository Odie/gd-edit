(ns gd-edit.equation-eval
  (:require [clojure.walk]))


(defn- parse-number
  "Reads a number from a string. Returns nil if not a number."
  [s]

  (when (re-find #"^-?\d+\.?\d*$" s)
    (read-string s)))

(defn- resolve-symbol
  [params sym]

  ;; Lookup and resolve a symbol if possible
  (if (contains? params sym)
    (params sym)
    sym)
  )

(defn- resolve-operator
  [operator-table op]

  (-> (operator-table op)
      (second)))

(defn- resolve-operand
  [params operand]

  (if (number? operand)
    operand

    (or (parse-number operand)
        (resolve-symbol params operand))))

(defn- eq->tokens
  [eq-str]
  (re-seq #"[\(\)\+\-\*\/\^]|\d+\.\d+|\d+|\w+" eq-str))

(defn- close-scope
  [stack]

  ;; Take the last item in the stack
  (let [item (last stack)
        popped-stack (pop stack)]

    ;; Append it the last item in the stack
    (update-in popped-stack [(dec (count popped-stack))] conj item)))

(defn- eq-tokens->ast
  [tokens]

  (loop [stack [[]]
         tokens tokens]
    (cond
      (empty? tokens)
      (do
        (assert (= 1 (count stack)))
        (first stack))

      (= (first tokens) "(")
      (recur (conj stack [])
             (rest tokens))

      (= (first tokens) ")")
      (recur (close-scope stack)
             (rest tokens))

      :else
      (recur (update-in stack [(dec (count stack))] conj (first tokens))
             (rest tokens)))))

;; Operator "string" => [precedence function]
(def basic-ops-table {"^" [0 #(Math/pow %1 %2)]
                      "*" [1 *]
                      "/" [1 /]
                      "+" [3 +]
                      "-" [3 -]})

(defn- order-ops
  "((A x B) y C) or (A x (B y C)) depending on precedence of x and y"
  [expression-list operator-table]

  ;; Find the highest precedence operator and its location
  (let [target (->> expression-list
                    (drop 1)
                    (take-nth 2)
                    (map-indexed (fn [index item] [item index]))
                    (sort-by (fn [[op]] (or (first (operator-table op)) (Integer/MAX_VALUE))))
                    (first))

        ;; Where the highest precedence operator?
        op-index (dec (* 2 (inc (second target))))

        subexp-start (dec op-index)
        subexp-end (+ op-index 2)

        ;; Extract the operator and the operands into a separate list
        subexp (subvec expression-list subexp-start subexp-end)

        expression-list' (into [] (-> (into [] (take subexp-start expression-list))
                                      (conj subexp)
                                      (concat (drop subexp-end expression-list))))
        ]

    (if (> (count expression-list') 3)
      (recur expression-list' operator-table)
      expression-list')))

(defn- ast-implement-op-precedence
  "Tree walk to group expressions based on operator precedence.
  All lists are length 3 afterwards."
  [ast precedence-table]

  (clojure.walk/postwalk
   #(if (coll? %)
      (let [c (count %)]
        (cond (even? c) (throw (Exception. "Must be an odd number of forms"))
              (= c 1) (first %)
              (= c 3) %
              (>= c 5) (order-ops % precedence-table)))
      %)
   ast))

(defn make-ast
  [equation-string]

  (-> equation-string
      (eq->tokens)
      (eq-tokens->ast)
      (ast-implement-op-precedence basic-ops-table)))

(defn eval-ast
  [ast params]

  (clojure.walk/postwalk
   (fn [item]
     (cond
       ;; If we're looking at an expression, evaluate it now
       (coll? item)
       (if (= 1 (count item))
         (first item)

         (do
           (assert (or (= (count item) 3)))

           (let [[term1 op term2] item
                 term1 (resolve-operand params term1)
                 term2 (resolve-operand params term2)
                 op (resolve-operator basic-ops-table op)]

             (op term1 term2))))


       ;; Ignore all items that are not expressions
       :else
       item))
   ast))

(defn evaluate
  ([equation-string]
   (evaluate equation-string {}))

  ([equation-string params]
   (eval-ast (make-ast equation-string) params)))
