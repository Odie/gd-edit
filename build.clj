(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell]
            [clojure.data.xml :as xml]
            [me.raynes.fs :as fs]
            )
  (:import [java.nio.file Path Paths]
           [java.io FileOutputStream]
           )
  )

(def project 'gd-edit)
(def lib 'gd-edit/core)
(def version (format "0.2.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format "target/%s-standalone.jar" (name project) version))

(defn shell
  [cmd-str]
  (apply clojure.java.shell/sh (str/split cmd-str #" ")))

(defn shell-stream
  [cmd-str]
  (.waitFor (-> (ProcessBuilder. (str/split cmd-str #" ")) .inheritIO .start)))

(defn git-sha
  []
  (str/trim ((shell "git rev-parse --short HEAD") :out)))

(defn git-tag
  []
  (str/trim ((shell "git describe --abbrev=0 --tags HEAD") :out))
  )

(defn git-branch
  []
  (str/trim ((shell "git rev-parse --abbrev-ref HEAD") :out)))

(defn make-build-info
  ([]
   (make-build-info {}))

  ([kv]
   (merge
    {:app-name project
     :version version

     :sha (git-sha)
     :tag (git-tag)
     :branch (git-branch)
     :timestamp (clojure.instant/read-instant-timestamp ((shell "git log -1 --pretty=format:%cd --date=iso-strict") :out))}
    kv)))

(defn- to-four-places
  [version]
  (let [places (str/split version #"\.")]
    (try
      (doall (map #(Integer/parseInt %) places))
      (if (> (count places) 4) (throw (NumberFormatException.)))
      (catch NumberFormatException e
        (throw (Exception. "Version string must consist of up to four numbers, separated by periods."))))
    (str/join "." (concat places (repeat (- 4 (count places)) "0")))))

(defn- filename-without-path
  [file]
  (let [path (Paths/get (.getPath file) (into-array [""]))]
    (str (.getFileName path))))

(defn launch4j-config
  [{:keys [out-file jar-file main-class
           project-name description version copyright
           jvm-opts jre-path]}]
  (xml/sexp-as-element
   [:launch4jConfig
    [:headerType "console"]
    [:outfile (.getAbsolutePath out-file)]
    [:jar (.getAbsolutePath jar-file)]
    [:classPath
     [:mainClass main-class]]
    (into [:jre
           (if jre-path
             [:path jre-path]
             [:path])
           [:minVersion "1.7.0"]
           [:maxHeapSize "2048"]
           [:jdkPreference "preferJdk"]]
          (for [jvm-opt jvm-opts]
            [:opt jvm-opt]))
    [:versionInfo
     [:fileVersion (to-four-places version)]
     [:txtFileVersion version]
     [:fileDescription description]
     [:copyright copyright]
     [:productVersion (to-four-places version)]
     [:txtProductVersion version]
     [:productName project-name]
     [:internalName project-name]
     [:originalFilename (filename-without-path out-file)]]]))

(defn write-launch4j-config
  [opts file-writer]
  (xml/emit (launch4j-config opts) file-writer))

(defn build-exe
  [{:keys [uber-file project-name main description version copyright jvm-opt jre-path]}]

  (let [jar    (b/resolve-path uber-file)
        tmp    (fs/temp-dir "gd-edit-build-exe")

        fname  (->> (.getName jar)
                    (re-matches #"(.+)\.jar")
                    second)

        xml-fname  (str fname ".xml")
        exe-fname  (str fname ".exe")
        xml-file   (io/file tmp xml-fname)
        out-file   (io/file (.getParent (io/file uber-file)) exe-fname)
        ]
    (with-open [xml (io/writer xml-file :encoding "UTF-8")]
      (write-launch4j-config {:jar-file     jar
                              :out-file     out-file
                              :main-class   main
                              :project-name project-name
                              :description  description
                              :version      version
                              :copyright    copyright
                              :jvm-opts     (or jvm-opt #{})
                              :jre-path     jre-path}
                             xml))
    (shell-stream (format "launch4j %s" (.getPath xml-file)))
    )
  )

(defn- bin-header
  [jvm-opts]
  (format "#!/bin/sh\n\nexec java %s -jar $0 \"$@\"\n\n\n"
          (str/join \space jvm-opts)))

(defn build-bin
  [{:keys [uber-file project-name main description version copyright jvm-opt jre-path header]}]
    (let [jar (b/resolve-path uber-file)
          tmp (fs/temp-dir "gd-edit-build-bin")
          header (if header
                   (slurp header)
                   (bin-header (or jvm-opt #{})))

          bin-fname  (->> (.getName jar)
                          (re-matches #"(.+)\.jar")
                          second)
          tgt-file   (io/file (.getParent (io/file uber-file)) bin-fname)
          ]
      (println (format "Creating %s binary..." bin-fname))
      (with-open [bin (FileOutputStream. tgt-file)]
        (io/copy header bin)
        (io/copy jar bin))
      (.setExecutable tgt-file true false)
      ))

(comment
  (build-exe {:uber-file uber-file
              :main 'gd_edit.core
              :jvm-opt #{"-Xms128m" "-Djna.nosys=true"}
              :project-name project
              :description "GrimDawn save game editor"
              :copyright "2022"
              :version version
              }
             )

  (build-bin {:uber-file uber-file
              :main 'gd_edit.core
              :jvm-opt #{"-Xms128m" "-Djna.nosys=true"}
              :project-name project
              :description "GrimDawn save game editor"
              :copyright "2022"
              :version version
              }
             )

  )

(def build-stage-atom (atom {:count 0}))
(defn print-build-stage
  [text]

  (swap! build-stage-atom update :count inc)
  (println (format "[%d] %s" (:count @build-stage-atom) text)))

(defn clean
  [_]
  (print-build-stage "Cleaning old build files...")
  (b/delete {:path "target"}))

(defn uber
  [_]
  (clean nil)

  ;; Pack build info as "build.edn" with the jar
  (print-build-stage "Creating build.edn...")
  (b/write-file {:path (format "%s/build.edn" class-dir)
                 :content (make-build-info)})

  (print-build-stage "Copying sources...")
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})

  (print-build-stage "Compilng sources...")
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir})

  (print-build-stage "Building uberjar...")
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'gd-edit.core})
  )

(defn build
  [_]
  (uber {})

  (let [settings {:uber-file uber-file
                  :main 'gd_edit.core
                  :jvm-opt #{"-Xms128m" "-Djna.nosys=true"}
                  :project-name project
                  :description "GrimDawn save game editor"
                  :copyright "2022"
                  :version version}]
    (print-build-stage "Building nix binary...")
    (build-bin settings)

    (print-build-stage "Building exe...")
    (build-exe settings))
  )

(comment

  (uber {})
  (build {})

  )
