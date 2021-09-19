(ns gd-edit.jline
  (:require [gd-edit.utils :as u])
  (:import [jline.console ConsoleReader]))

(def ^:redef use-jline false)
(def ^:redef reader nil)

(defn initialize
  []
  (alter-var-root #'use-jline (constantly true))
  (alter-var-root #'reader (constantly (ConsoleReader.))))

(defn set-input
  [input-stream]
  (u/call-method reader "setInput" input-stream))

(defn readline
  ([]
   (readline ""))
  ([prompt]

  ;; For some reason, using calling jline's readline throws an exception
  ;; when invoked from the cider repl
  (if (not use-jline)
    (do
      (u/print- prompt)
      (clojure.core/read-line))

    (.readLine reader prompt))))
