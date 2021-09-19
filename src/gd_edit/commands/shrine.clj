(ns gd-edit.commands.shrine
  (:require [gd-edit.globals :as globals]
            [gd-edit.db-utils :as dbu]
            [gd-edit.utils :as u]
            [jansi-clj.core :refer [red green yellow bold black]]
            [gd-edit.printer :as printer]
            [clojure.string :as str]
            [com.rpl.specter :as s]))

(defn shrine-list-handler
  [[_ _]]

  (let [shrines (dbu/get-shrines)]
    (doseq [shrine shrines]
      (u/print-indent 1)
      (u/print-line (yellow (:display-name shrine)) (:recordname shrine)))

    (u/print-line)
    (u/print-line (count shrines) "shrine UIDs total")))

(defn gate-list-handler
  [[_ _]]

  (let [gates (dbu/get-gates)]
    (doseq [gate gates]
      (u/print-indent 1)
      (u/print-line (yellow (:display-name gate)) (:recordname gate)))

    (u/print-line)
    (u/print-line (count gates) "gate UIDs total")))
