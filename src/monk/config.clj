(ns monk.config
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]))

(defn- parent-dirs
  [^String root]
  (->> (.getAbsoluteFile (io/file root))
    (iterate #(.getParentFile ^java.io.File %))
    (take-while some?)))

(defn- find-config-file-in-dir
  ^java.io.File [^java.io.File dir]
  (let [f (io/file dir ".monk.edn")]
    (when (.exists f)
      f)))

(defn- cli-file-reader
  [filepath]
  (let [contents (slurp filepath)]
    (edn/read-string contents)))

(defn load-from-file
  []
  (some->> (parent-dirs "")
           (some find-config-file-in-dir)
           (#(.getPath ^java.io.File %))
           cli-file-reader))
