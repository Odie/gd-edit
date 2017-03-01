(ns gd-edit.core-test
  (:require [midje.sweet :refer :all]  
            [gd-edit.core :refer :all]))

(let [input "Hello world"]
  (fact "tokenization should return the input itself as the first value"

        (first (#'gd-edit.core/tokenize-input input))
        => input))

(let [input "Hi \"This is a long string\""]
  (fact "tokenization extracts quoted strings"
        (#'gd-edit.core/tokenize-input input)
        => [input '("Hi" "This is a long string")]))

