(ns gd-edit.globals)

(def menu (atom {:display-fn nil
                 :choice-map nil}))

(def db (atom {}))
(def query-state
  (atom {:query-string nil
         :result nil
         :page 0
         :pagination-size 10}))

(def character (atom{}))
