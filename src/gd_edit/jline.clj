(ns gd-edit.jline
  (:import [jline.console ConsoleReader]))

(def use-jline false)
(def reader nil)

(defn initialize
  []
  (alter-var-root #'use-jline (constantly true))
  (alter-var-root #'reader (constantly (ConsoleReader.))))

(defn readline
  ([]
   (readline ""))
  ([prompt]

  ;; For some reason, using calling jline's readline throws an exception
  ;; when invoked from the cider repl
  (if (not use-jline)
    (do
      (print prompt)
      (clojure.core/read-line))

    (.readLine reader prompt))))
