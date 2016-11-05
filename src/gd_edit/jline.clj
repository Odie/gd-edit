(ns gd-edit.jline
  (:import  [org.jline.reader LineReaderBuilder]
            [org.jline.terminal TerminalBuilder]))

(def use-jline false)
(def reader nil)

(defn initialize
  []
  (let [terminal (-> (TerminalBuilder/builder)
                     (.build))]

    (def reader (-> (LineReaderBuilder/builder)
                    (.terminal terminal)
                    (.build)))))

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

      (.readLine reader prompt nil nil nil))))


#_(initialize)
