(ns gd-edit.qst-reader-test
  (:require [gd-edit.qst-reader :as sut]
            [midje.sweet :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [gd-edit.utils :as u]))


;;------------------------------------------------------------------------------
;; Try loading some test quest files
;;------------------------------------------------------------------------------
(let [test-file (-> (io/resource "quest-def-files")
                    (io/file "test.qst"))]

  ;;----------------------------------------------------------------------------
  ;; There isn't a very good way to test qst reading at the moment.
  ;;
  ;; The best we can do now is to hard code the expected decoded content of
  ;; a small test file to hopefully exercise most of the file.
  ;;----------------------------------------------------------------------------
  (let [quest-def (sut/load-quest-file test-file)]
    (fact "Verify content of the sole test file"
          quest-def
          =>   {:quest
                {:static/type :quest,
                 :uid -1827459072,
                 :flags 0,
                 :tasks
                 [{:description "The description goes here",
                   :dont-propegate 0,
                   :uid 2065181184,
                   :on-accept
                   [{:static/type :quest-event,
                     :flags 0,
                     :conditions
                     {:oper 0,
                      :conditions
                      '({:type 2,
                         :version 0,
                         :comparision 3,
                         :static/type :has-experience,
                         :amount 1000})},
                     :actions
                     [{:type 12, :version 0, :static/type :debug-print, :text "123"}],
                     :text ""}],
                   :objectives
                   [{:static/type :quest-objective,
                     :uid 2047927680,
                     :flags 0,
                     :conditions
                     {:oper 0,
                      :conditions
                      '({:type 2,
                         :version 0,
                         :comparision 3,
                         :static/type :has-experience,
                         :amount 2000})},
                     :actions
                     [{:type 12, :version 0, :static/type :debug-print, :text "456"}],
                     :text ""}],
                   :is-blocker 0,
                   :static/type :quest-task,
                   :flags 0,
                   :on-complete
                   [{:static/type :quest-event,
                     :flags 0,
                     :conditions
                     {:oper 0,
                      :conditions
                      '({:type 2,
                         :version 0,
                         :comparision 3,
                         :static/type :has-experience,
                         :amount 3000})},
                     :actions
                     [{:type 12, :version 0, :static/type :debug-print, :text "789"}],
                     :text ""}],
                   :text "The task name"}
                  {:description "",
                   :dont-propegate 0,
                   :uid -792428032,
                   :on-accept [],
                   :objectives [],
                   :is-blocker 0,
                   :static/type :quest-task,
                   :flags 0,
                   :on-complete [],
                   :text "Task 2"}],
                 :text ""},
                :string-tables
                [{:name "enUS",
                  :tags
                  [""
                   ""
                   "The task name"
                   "The description goes here"
                   ""
                   ""
                   ""
                   "Task 2"
                   ""]}]})))
