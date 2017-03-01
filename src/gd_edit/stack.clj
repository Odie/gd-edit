(ns gd-edit.stack
  (:refer-clojure :rename {pop! core-pop!}))

;;--------------------------------------------------------------------
;; Stack functions
;;
;; Simple functions that makes dealing with stack less verbose
(defn push!
  [stack-atom item]

  (when-not (nil? item)
    (swap! stack-atom conj item)))

(defn pop!
  "Pop an item off of the stack"
  [stack-atom]

  (when-not (empty? @stack-atom)
    (let [item (peek @stack-atom)]
      (swap! stack-atom pop)
      item)))

(defn pop-safe
  [coll]
  (if-not (empty? coll)
    (pop coll)
    coll))

(defn replace-last!
  [stack-atom item]

  (when-not (nil? item)
    (swap! stack-atom (fn [stack]
                        (-> stack
                            (pop-safe)
                            (conj item))))))


