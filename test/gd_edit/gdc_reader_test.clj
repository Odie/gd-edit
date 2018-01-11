(ns gd-edit.gdc-reader-test
  (:require [midje.sweet :refer :all]
            [gd-edit.gdc-reader :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [boot.core]

            [gd-edit.utils :as u]
            ))

;;------------------------------------------------------------------------------
;; Try loading some test save files
;;------------------------------------------------------------------------------

(doseq [save-file (->> (io/resource "save-files")
                       (io/file)
                       (file-seq)
                       (filter #(str/ends-with? % ".gdc")))]

  ;;----------------------------------------------------------------------------
  ;; Basic sanity checks
  ;;
  ;; This gives some degree of confidence that reading is working
  ;;----------------------------------------------------------------------------
  (let [character (load-character-file save-file)]

    (fact (format "Loading valid gdc file should return a map: \"%s\"" save-file)
          (map? character)
          => true)

    (fact "Loaded character should have a name"
          (string? (:character-name character))
          => true)

    ;;--------------------------------------------------------------------------
    ;; Verify reading & writing produces the same content
    ;;
    ;; This should indicate both the reading and writing code is working
    ;; correctly.
    ;;--------------------------------------------------------------------------
    (let [src save-file
          dst (java.io.File/createTempFile "character" ".gdc")]

      (write-character-file character dst)

      (fact "Loading and writing file should produce identical content"
            (= (u/md5-file src) (u/md5-file dst))
            => true))))
