(def project 'gd-edit)
(def version "0.1.2-SNAPSHOT")

(set-env! :resource-paths #{"resources" "src"}
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "RELEASE"]
                            [org.clojure/tools.namespace "0.2.11" :scope "test"]
                            [net.jpountz.lz4/lz4 "1.3.0"]
                            [jline/jline "2.14.2"]
                            [org.fusesource.jansi/jansi "1.14"]
                            [jansi-clj "0.1.0"]
                            [adzerk/boot-jar2bin "1.1.0" :scope "test"]
                            [adzerk/boot-test "RELEASE" :scope "test"]
                            [net.java.dev.jna/jna "4.2.2"]
                            [net.java.dev.jna/jna-platform "4.2.2"]
                            [spyscope "0.1.5" :scope "test"]
                            [fipp "0.6.8"]
                            [org.clojure/core.async "0.2.395"]])



(require '[adzerk.boot-jar2bin :refer :all]
         '[clojure.java.shell]
         '[clojure.string :as str]
         ;; '[clj-time.format :as tf]
         ;; '[clj-time.core :as t]
         )

(task-options!
 aot {:namespace   #{'gd-edit.core}}
 pom {:project     project
      :version     version
      :description "FIXME: write description"
      :url         "http://example/FIXME"
      :scm         {:url "https://github.com/yourname/gd-edit"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}}
 jar {:main        'gd-edit.core
      :file        (str "gd-edit-" version "-standalone.jar")}
 bin {:jvm-opt #{"-Xms128m" "-Xmx512m"}}
 exe {:jvm-opt #{"-Xms128m" "-Xmx512m"}
      :name      project
      :main      'gd_edit.core
      :version   "0.1.0"
      :desc      "GrimDawn save game editor"
      :copyright "2016"}
 )

;;------------------------------------------------------------------------------
;;
;; Shell utilities
;;

(defn shell
  [cmd-str]
  (apply clojure.java.shell/sh (str/split cmd-str #" ")))

(defn make-build-info
  ([]
   (make-build-info {}))

  ([kv]
   (merge
    {:sha (str/trim ((shell "git rev-parse --short HEAD") :out))
     :tag (str/trim ((shell "git describe --abbrev=0 --tags HEAD") :out))
     :branch (str/trim ((shell "git rev-parse --abbrev-ref HEAD") :out))
     :timestamp (new java.util.Date)} kv)))

;;------------------------------------------------------------------------------
;;
;; Tasks
;;
(deftask build-info []
  (with-pre-wrap fs
    (let [t (tmp-dir!)]
      (spit (clojure.java.io/file t "build.edn") (make-build-info {:app-name project}))
      (-> fs (add-resource t) commit!))))

(deftask cider "CIDER profile"
  []
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[org.clojure/tools.nrepl "0.2.12"]
                  [cider/cider-nrepl "0.14.0"]
                  [refactor-nrepl "2.2.0"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware
                  refactor-nrepl.middleware/wrap-refactor])
  identity)

(deftask build
  "Build the project locally as a JAR."
  [d dir PATH #{str} "the set of directories to write to (target)."]
  (let [dir (if (seq dir) dir #{"target"})]
    (comp (build-info) (aot) (pom) (uber) (jar) (target :dir dir) (exe :output-dir "target") (bin :output-dir "target"))))

(deftask run
  "Run the project."
  [a args ARG [str] "the arguments for the application."]
  (require '[gd-edit.core :as app])
  (apply (resolve 'app/-main) args))

(deftask dev
  []
  (comp (cider) (launch-nrepl) (run)))

(require '[adzerk.boot-test :refer [test]])
