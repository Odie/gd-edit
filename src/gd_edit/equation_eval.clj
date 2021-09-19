(ns gd-edit.equation-eval
  (:require [clojure.walk]
            [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

;; Build a parser from the grammar
(insta/defparser parser
  (io/resource "equation.grammar")
  :auto-whitespace :standard)

(defn evaluate
  "Evaluate the equation"
  ([equation-string]
   (evaluate equation-string {}))

  ([equation-string variables]
   ;; (u/print-line "Evaluating:" equation-string)
   (->> (parser equation-string)
        (insta/transform
         {:add +
          :sub -
          :mul *
          :div /
          :exp #(Math/pow %1 %2)
          :identifier #(variables %1)
          :integer edn/read-string
          :float clojure.edn/read-string
          :expr identity}))))
