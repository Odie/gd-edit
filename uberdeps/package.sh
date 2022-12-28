#!/bin/bash -e
START_DIR="$(pwd)"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_DIR="$(git rev-parse --show-toplevel)"

echo "* COMPILING..."
cd $PROJECT_DIR
clojure -J-Dclojure.compiler.direct-linking=true "-J-Dclojure.compiler.elide-meta=[:doc :file :line :added]" -e "(compile 'gd-edit.core)"

echo "* UBERJARRING..."
cd $SCRIPT_DIR
# clojure -m uberdeps.uberjar --deps-file ../deps.edn --target ../target/project.jar --aliases package
clojure -A:uberjar --deps-file ../deps.edn --target ../target/project.jar --aliases package --main-class gd_edit.core
