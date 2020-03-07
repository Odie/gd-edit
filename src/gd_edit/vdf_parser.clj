(ns gd-edit.vdf-parser
  "Provides the ability to parse steam vdf files into a clojure-friendly hashmap."
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk]))

(defn- strip-quotes
  [s]
  (str/replace s #"^\"|\"$" ""))

(defn- file?
  [obj]
  (= java.io.File (type obj)))

(defn- vdf-tree->map
  "Transforms the output of instaparse into a clojure hashmap"
  [tree]

  ;; Do nothing if the we didn't get a valid tree
  (if (= instaparse.gll.Failure (type tree))
    tree

    (clojure.walk/postwalk (fn [node]
                             (if-not (vector? node)
                               node

                               (let [[node-type data] node]
                                 (condp = node-type
                                   :name (strip-quotes data)
                                   :value (strip-quotes data)
                                   :entry [(nth node 1) (nth node 2)]
                                   :block (into {} (rest node))
                                   :vdf {(nth node 1) (nth node 2)}
                                   node))))
                           tree)))

(def parser
  (insta/parser
   (slurp (io/resource "vdf.grammar"))
   :auto-whitespace :standard))

(defn parse
  [file]

  (when-let [content (cond
                       (file? file) (slurp file)
                       (string? file) file
                       :else nil)]

    (->> (parser content)
         (vdf-tree->map))))

(comment

   (->> "/Volumes/Untitled/Program Files (x86)/Steam/steamapps/libraryfolders.vdf"
        (io/file)
        parse)

)
