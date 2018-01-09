(ns gd-edit.commands.diag
  (:require [clojure.string :as str]
            [jansi-clj.core :refer :all]
            [gd-edit.game-dirs :as dirs]
            [clojure.java.io :as io]
            ))

(defn- print-test-header
  [result-bool text]

  (if result-bool
    (println (green (str "✔ " text)))
    (println (red   (str "✖ " text)))))

(defn- verify-jvm-version
  []

  (let [jvm-version (System/getProperty "java.runtime.version")
        result (str/starts-with? jvm-version "1.8")
        test-info (str "JVM version: " jvm-version)]

    (print-test-header result test-info)
    result))

(defn- verify-game-dir-present
  []

  (let [result (some-> (dirs/get-game-dir)
                       (io/file)
                       (.exists))
        test-info (if result
                    (str "Game directory exists")
                    (str "Game directory does not exist"))]

    (print-test-header result test-info)
    result))

(defn verify-file-exists
  [file-path]

  (let [result (some-> file-path
                       (io/file)
                       (.exists))
        test-info (if result
                    (str "File exists: " file-path)
                    (str "File does not exist: " file-path))]

    (print-test-header result test-info)
    result))

(defn- test-passes?
  [test-fn-call]

  (if (apply (first test-fn-call) (rest test-fn-call))
    true
    false))

(defn- verify-test
  [[test-fn-call err-msg]]

  (if (test-passes? test-fn-call)
    true
    (do (println err-msg)
        (newline)
        false)))

(defn diag-handler
  [[input tokens]]

  (let [tests [[[verify-jvm-version]
                (yellow "Please make sure you're running Java 1.8 or above")
                ]

               [[verify-game-dir-present]
                (yellow "Please use the 'gamedir' command to help the editor find your game installation directory")]

               [[verify-file-exists (io/file (dirs/get-game-dir) dirs/database-file)]
                (yellow "Cannot find file: database/database.arz. Please set the gamedir properly and make sure all GD game files are present in the game installation directory")]

               [[verify-file-exists (io/file (dirs/get-game-dir) dirs/localization-file)]
                (yellow "Cannot find file: Text_EN.arc. Please set the gamedir properly and make sure all GD game files are present in the game installation directory")]

               [[verify-file-exists (io/file (dirs/get-game-dir) dirs/texture-file)]
                 (yellow "Cannot find file: Items.arc. Please set the gamedir properly and make sure all GD game files are present in the game installation directory")]]

        all-tests-passed (every? verify-test tests)]

    (newline)
    (if all-tests-passed
      (println "Looks good! The editor should be ready to go!")
      (println "Oops! Please fix those errors before using the editor!"))))
