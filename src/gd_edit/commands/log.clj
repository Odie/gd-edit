(ns gd-edit.commands.log
  (:require [jansi-clj.core :refer [red green yellow]]
            [gd-edit.utils :as u]
            [gd-edit.globals :as globals]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

(defn- log-level-get
  [settings]

  (:log-level settings))

(defn- log-level-set
  [settings log-level]

  (assoc settings :log-level log-level))

(defn- log-level-clear
  [settings]

  (dissoc settings :log-level))

(defn log-handler
  [[input token]]

  ;; If the user supplied no params, show the current log level
  (if (zero? (count token))
    (do
      (u/print-line "Current log level:")
      (u/print-indent 1)
      (u/print-line
       (yellow
        (u/keyword->str
         (or (log-level-get @globals/settings)
             :info)))))

    (let [log-level-str (str/lower-case (first token))]
      (cond
        ;; Did the user asked to stop logging?
        (= log-level-str "clear")
        (do
          (swap! globals/settings log-level-clear)
          (u/print-line (green "Ok!")))

        ;; The user asked to set a specific log level?
        (contains? log/-levels-set (keyword log-level-str))
        (let [log-level (keyword log-level-str)]
          (swap! globals/settings log-level-set (keyword log-level))
          (log/set-level! log-level)
          (u/print-line (green "Ok!")))

        :else
        (u/print-line (red "Unknown log level: " log-level-str))))))
