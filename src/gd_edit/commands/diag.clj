(ns gd-edit.commands.diag
  (:require [clojure.string :as str]
            [gd-edit.game-dirs :as dirs]
            [clojure.java.io :as io]
            [jansi-clj.core :refer [red green yellow]]))

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

    [result test-info]))

(defn- verify-game-dir-present
  []

  (let [result (some-> (dirs/get-game-dir)
                       (io/file)
                       (.exists))
        test-info (if result
                    (str "Game directory exists")
                    (str "Game directory does not exist"))]

    ;;(print-test-header result test-info)
    [result test-info]))

(defn verify-file-exists
  [file-path]

  (let [result (some-> file-path
                       (io/file)
                       (.exists))
        test-info (if result
                    (str "File exists: " file-path)
                    (str "File does not exist: " file-path))]

    [result test-info]))

(defn- test-passes?
  [test-fn-call]

  (apply (first test-fn-call) (rest test-fn-call)))

(defn- verify-test
  [[test-fn-call additional-message]]

  (let [[passed? test-msg] (test-passes? test-fn-call)]
    (cond-> {:test-fn-call test-fn-call
             :passed? passed?
             :test-msg test-msg}
      (not passed?) (assoc :additional-message additional-message))))

(defn print-failed-diag-tests
  [test-results]
  (doseq [test-result test-results]
    (when-not (:passed? test-result)
      (print-test-header (:passed? test-result) (:test-msg test-result))
      (println (:additional-message test-result)))))

(defn diag-info
  []

  (let [tests [[[verify-jvm-version]
                (yellow "Please make sure you're running Java 1.8 or above")]

               [[verify-game-dir-present]
                (yellow "Please use the 'gamedir' command to help the editor find your game installation directory")]

               [[verify-file-exists (io/file (dirs/get-game-dir) dirs/database-file)]
                (yellow "Cannot find file: database/database.arz. Please set the gamedir properly and make sure all GD game files are present in the game installation directory")]

               [[verify-file-exists (io/file (dirs/get-game-dir) dirs/localization-file)]
                (yellow "Cannot find file: Text_EN.arc. Please set the gamedir properly and make sure all GD game files are present in the game installation directory")]

               [[verify-file-exists (io/file (dirs/get-game-dir) dirs/texture-file)]
                (yellow "Cannot find file: Items.arc. Please set the gamedir properly and make sure all GD game files are present in the game installation directory")]]]

    (map verify-test tests)))



(defn diag-handler
  [[input tokens]]

  (let [test-results (diag-info)
        all-tests-passed (every? :passed? test-results)]

    (doseq [test-result test-results]
      (print-test-header (:passed? test-result) (:test-msg test-result))
      (when-not (:passed? test-result)
        (println (:additional-message test-result))))

    (newline)
    (if all-tests-passed
      (println "Looks good! The editor should be ready to go!")
      (println "Oops! Please fix those errors before using the editor!"))))
