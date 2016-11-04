(ns gd-edit.globals)

(def db (atom {}))
(def query-state
  (atom {:result nil
         :page 0
         :pagination-size 10}))
