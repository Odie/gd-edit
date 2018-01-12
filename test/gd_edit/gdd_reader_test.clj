(ns gd-edit.gdd-reader-test
  (:require [gd-edit.gdd-reader :as sut]
            [midje.sweet :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [gd-edit.utils :as u]))


;;------------------------------------------------------------------------------
;; Try loading some test quest files
;;------------------------------------------------------------------------------

(doseq [test-file (->> (io/resource "quest-progress-files")
                       (io/file)
                       (file-seq)
                       (filter #(str/ends-with? % ".gdd")))]

  ;;----------------------------------------------------------------------------
  ;; Basic sanity checks
  ;;
  ;; This gives some degree of confidence that reading is working
  ;;----------------------------------------------------------------------------
  (let [quest-progress (sut/load-quest-file test-file)]

    ;;--------------------------------------------------------------------------
    ;; Verify reading & writing produces the same content
    ;;
    ;; This should indicate both the reading and writing code is working
    ;; correctly.
    ;;--------------------------------------------------------------------------
    (let [src test-file
          dst (java.io.File/createTempFile "quest" ".gdd")]

      (sut/write-quest-file quest-progress dst)

      (fact "Loading and writing file should produce identical content"
            (= (u/md5-file src) (u/md5-file dst))
            => true))))
