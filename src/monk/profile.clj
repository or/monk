(ns monk.profile
  (:require
   [clj-async-profiler.core :as prof]
   [monk.main :as main]
   [monk.tool :as tool]))

(defn -main
  [& args]
  (binding [tool/*profiling* true]
    (prof/profile (apply main/-main args))))
