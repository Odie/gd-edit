(def project 'gd-edit)
(def version "0.1.0-SNAPSHOT")

(set-env! :resource-paths #{"src"}
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "RELEASE"]
                            [net.jpountz.lz4/lz4 "1.3.0"]
                            [org.jline/jline "3.0.1"]
                            [org.fusesource.jansi/jansi "1.14"]
                            [jansi-clj "0.1.0"]
                            [adzerk/boot-jar2bin "1.1.0" :scope "test"]
                            [adzerk/boot-test "RELEASE" :scope "test"]])

(require '[adzerk.boot-jar2bin :refer :all])

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

(deftask build
  "Build the project locally as a JAR."
  [d dir PATH #{str} "the set of directories to write to (target)."]
  (let [dir (if (seq dir) dir #{"target"})]
    (comp (aot) (pom) (uber) (jar) (target :dir dir) (exe :output-dir "target") (bin :output-dir "target"))))

(deftask run
  "Run the project."
  [a args ARG [str] "the arguments for the application."]
  (require '[gd-edit.core :as app])
  (apply (resolve 'app/-main) args))

(require '[adzerk.boot-test :refer [test]])
