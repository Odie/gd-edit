(def project 'gd-edit)
(def version "0.1.6-SNAPSHOT")

(set-env! :resource-paths #{"resources" "src"}
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "1.9.0-alpha14"]
                            [org.clojure/tools.namespace "0.2.11" :scope "test"]
                            [net.jpountz.lz4/lz4 "1.3.0"]
                            [jline/jline "2.14.2"]
                            [org.fusesource.jansi/jansi "1.16"]
                            [jansi-clj "0.1.0"]
                            [adzerk/boot-jar2bin "1.1.0" :scope "test"]
                            [adzerk/boot-test "RELEASE" :scope "test"]
                            [net.java.dev.jna/jna "4.2.2"]
                            [net.java.dev.jna/jna-platform "4.2.2"]
                            [spyscope "0.1.6" :scope "test"]
                            ;; [fipp "0.6.8"]
                            [org.clojure/core.async "0.2.395"]
                            [progress "1.0.2"]
                            [com.dropbox.core/dropbox-core-sdk "2.1.2" :scope "test"]

                            [org.clojure/data.json "0.2.6" :scope "test"]
                            [digest "1.4.5" :scope "test"]
                            [me.raynes/fs "1.4.6"]
                            [midje "1.9.0-alpha6" :scope "test"]
                            [zilti/boot-midje "0.2.2-SNAPSHOT" :scope "test"]
                            [clj-http "2.3.0"]
                            [com.taoensso/timbre "4.8.0"]])



(require '[adzerk.boot-jar2bin :refer :all]
         '[clojure.java.io :as io]
         '[clojure.java.shell]
         '[clojure.string :as str]
         '[clojure.data.json]
         '[clojure.edn :as edn]
         '[boot.core :as c]
         '[boot.util :as u]
         '[digest]
         '[me.raynes.fs :as fs]
         '[zilti.boot-midje :refer [midje]]
         )

(import com.dropbox.core.DbxRequestConfig
        com.dropbox.core.v2.DbxClientV2
        com.dropbox.core.v2.files.WriteMode)

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
 bin {:jvm-opt #{"-Xms128m" "-Xmx1024m"}}
 exe {:jvm-opt #{"-Xms128m" "-Xmx1024m"}
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
    {:app-name project
     :version version

     :sha (str/trim ((shell "git rev-parse --short HEAD") :out))
     :tag (str/trim ((shell "git describe --abbrev=0 --tags HEAD") :out))
     :branch (str/trim ((shell "git rev-parse --abbrev-ref HEAD") :out))
     :timestamp (clojure.instant/read-instant-timestamp ((shell "git log -1 --pretty=format:%cd --date=iso-strict") :out))}
    kv)))

;;------------------------------------------------------------------------------
;;
;; Tasks
;;
(deftask build-info []
  (with-pre-wrap fs
    (let [t (tmp-dir!)]
      (spit (clojure.java.io/file t "build.edn") (make-build-info))
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

(defn add-test-resources!
  []

  (set-env! :resource-paths (fn [oldval]
                              (if (contains? oldval "test-resources")
                                oldval
                                (conj oldval "test-resources")))))

(deftask test
  []

  (add-test-resources!)
  (comp (midje)))

(deftask autotest
  []

  (add-test-resources!)
  (comp (watch) (test) (speak)))

(defn- await-exe-build
  [exe-file jar-file]

  ;; Wait until the exe exists
  (while (not (.exists exe-file))
    (println "Exe doesn't seem to exist yet")
    (Thread/sleep 100))

  ;; Wait until the exe to be built
  (loop [last-length 0]
    (let [cur-length (.length exe-file)]

      ;; The exe file is at least as large as the jar file
      ;; and that the file has stopped growing
      (if (and (> (.length exe-file) (.length jar-file))
               (= cur-length last-length))

        ;; Then we are done
        :done

        ;; Otherwise, we need to wait for the exe to finish building
        (do
          ;; Sleep for a bit and try again...
          (println "Waiting for exe to finish building...")
          (Thread/sleep 100)
          (recur cur-length)))))

  (assert (> (.length exe-file) (.length jar-file))))

(defn- make-dropbox-client
  []
  (let [config (DbxRequestConfig. "GDUploader/0.1" "en_US")
        dropbox-setting-file (edn/read-string (slurp (io/file ".publish.edn")))]
    (DbxClientV2. config (:token dropbox-setting-file))))

(defn- git-has-uncommitted-changes
  []

  (if (= (:exit (shell "git diff-index --quiet HEAD --")) 1)
    true
    false))

(defn make-bin-edn-pair-name
  [upload-filename]


  {:filename
   :edn-filename
   }
  )

(defn replace-path-extension
  [path new-ext]

  (as-> (io/file path) $
    (fs/base-name $ true)
    (io/file (fs/parent path) $)
    (str $ new-ext)))

(defn replace-base-name-extension
  [basename new-ext]

  (-> (io/file basename)
      (fs/base-name true)
      (str new-ext)))

(defn upload-bin-edn-pair-to-dropbox
  [dropbox-client bin-file upload-filename]

  (let [edn-base-name (replace-base-name-extension upload-filename ".edn")]

       ;; Write the gd-editor.exe file
       (println (format "Uploading %s build..." upload-filename))
       (with-open [exe-stream (io/input-stream bin-file)]
         (-> dropbox-client
             (.files)
             (.uploadBuilder (str (io/file "/Public/GrimDawn/editor/" upload-filename)))
             (.withMode WriteMode/OVERWRITE)
             (.uploadAndFinish exe-stream)))

       ;; Write the gd-editor.edn file to describe the latest version
       (println (format "Uploading %s edn file..." upload-filename))
       (-> dropbox-client
           (.files)
           (.uploadBuilder (str (io/file "/Public/GrimDawn/editor/" edn-base-name)))
           (.withMode WriteMode/OVERWRITE)
           (.uploadAndFinish (-> (make-build-info {:filesize (.length bin-file)
                                                   :file-sha1 (digest/sha1 bin-file)})
                                 (pr-str)
                                 (.getBytes)
                                 (io/input-stream))))))

(deftask publish-exe
  "Publish the built windows exe to dropbox"
  []

  (when (git-has-uncommitted-changes)
    (println "Please don't publish using uncommitted changes.\nThis makes the build info useless for determining what the user is running.")
    (throw (Throwable. "Should not publish uncommitted changes")))

  (let [tmp (c/tmp-dir!)]


    (with-post-wrap fileset
      (println "Publishing exe to dropbox...")

      ;; jar2bin doesn't play well with the boot pipeline...
      ;; Executing via the shell causes a separate process to be launched, which
      ;; boot isn't able to wait for.
      ;;
      ;; We're going to have to do something silly to figure out when the
      ;; exe generation is done
      (let [current-dir (System/getProperty "user.dir")
            output-dir (io/file current-dir "target")

            output-jar-file (io/file output-dir
                                     (str/join "-"
                                               [project version "standalone.jar"]))
            output-file (io/file output-dir
                                 (str/join "-"
                                           [project version "standalone.exe"]))]

        (await-exe-build output-file output-jar-file)

        (let [client (make-dropbox-client)]
          ;; Upload the windows exe
          (upload-bin-edn-pair-to-dropbox client
                                          output-file
                                          "gd-edit.exe")

          ;; Upload wrapped binary suitable for platforms
          (upload-bin-edn-pair-to-dropbox client
                                          (io/file (replace-path-extension output-file ""))
                                          "gd-edit.nix.bin")
          ))
      fileset)))

(deftask publish
  []
  (comp (build) (publish-exe)))

(-> (io/file "test.nix.bin")
     (fs/base-name true)
     (str ".edn")
     )
