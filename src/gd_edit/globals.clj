(ns gd-edit.globals
  (:require [clojure.core.async :as async]))

(def menu-stack (atom []))

;; Notification channel for the repl
(def notification-chan (async/chan))

(def db (atom {}))
(def db-index (atom {}))

(def query-state
  (atom {:query-string nil
         :result nil
         :page 0
         :pagination-size 10}))

(def character (atom{}))
(def last-loaded-character (atom{}))

(def settings (atom{}))
