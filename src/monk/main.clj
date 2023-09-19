(ns monk.main
  (:require
   [monk.core :as monk]))

(defn -main [& args]
  (monk/reformat-string "(ns   foo.bar
   (:require  [clojure.string :as  str]))"))
