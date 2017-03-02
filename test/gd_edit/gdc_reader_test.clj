(ns gd-edit.gdc-reader-test
  (:require [midje.sweet :refer :all]
            [gd-edit.gdc-reader :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [boot.core]
            ))

;;------------------------------------------------------------------------------
;; Try loading some test save files
;;------------------------------------------------------------------------------
(doseq [save-file (->> (io/resource "save-files")
                       (io/file)
                       (file-seq)
                       (filter #(str/ends-with? % ".gdc")))]

  (let [filename (-> save-file
                     (.toPath)
                     (.getFileName))

        character (load-character-file save-file)
        ]

    (fact (format "Loading valid gdc file should return a map: \"%s\"" filename)
          (map? character)
          => true)

    (fact "Loaded character should have a name"
     (string? (:character-name character))
     => true
     )))
