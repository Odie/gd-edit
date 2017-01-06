(ns gd-edit.jline
  (:import [jline.console ConsoleReader]))

(def use-jline false)
(def reader nil)

(defn initialize
  []

  (def reader (ConsoleReader.)))

(defn readline
  [prompt]

  ;; For some reason, using calling jline's readline throws an exception
  ;; when invoked from the cider repl
  (if (not use-jline)
    (do
      (print prompt)
      (clojure.core/read-line))

    (do
      (if (nil? reader)
        (initialize))

      (.readLine reader prompt))))


#_(initialize)
