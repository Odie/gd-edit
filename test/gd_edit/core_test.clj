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

(let [command-map {["command1"] 1
                   ["command2"] 2
                   ["command2" "sub-command"] 3
                   ["command3" "sub-command"] 4}]
  (fact
   (#'gd-edit.core/find-command ["command1"] command-map)
   => ["command1"]

   (#'gd-edit.core/find-command ["command2" "sub-command"] command-map)
   => ["command2" "sub-command"])

  (fact "commands must start with a base command then go on with subcommands"
   (#'gd-edit.core/find-command ["command3" "sub-command"] command-map)
   => ["command3" "sub-command"])
  )
