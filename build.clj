(ns build
  (:require
   [clojure.string :as str]
   [clojure.tools.build.api :as b]))

(def version
  (-> (slurp "resources/VERSION")
      str/trim
      (str/split #"\n")
      first))

(def class-dir
  "target/classes")

(def basis
  (b/create-basis {:project "deps.edn"}))

(def uber-file
  (format "target/monk-%s-standalone.jar" version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'monk.main}))
