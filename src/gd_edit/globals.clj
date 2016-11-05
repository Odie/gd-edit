(ns gd-edit.globals)

(def db (atom {}))
(def query-state
  (atom {:query-string nil
         :result nil
         :page 0
         :pagination-size 10
         :filter-max-fields []}))
