#!/usr/bin/evn bb
(ns gd-edit.build-info
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(defn make-build-info
  ([]
   (make-build-info {}))

  ([kv]
   (merge
    {:app-name project
     :version version

     :sha (str/trim (:out (sh "git rev-parse --short HEAD")))
     :tag (str/trim (:out (sh "git describe --abbrev=0 --tags HEAD")))
     :branch (str/trim (:out (shell "git rev-parse --abbrev-ref HEAD")))
     :timestamp (clojure.instant/read-instant-timestamp ((shell "git log -1 --pretty=format:%cd --date=iso-strict") :out))}
    kv)))

(defn -main
  [& args]
  (make-build-info))
