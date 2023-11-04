(ns monk.core
  (:require
   [clojure.zip :as z]
   [monk.ast :as ast]
   [monk.transform :as transform]
   [parcera.core :as parcera]))

(defn format-string
  [data {:keys [symbol-mapping]}]
  (-> (ast/parse data)
      ast/zipper
      (transform/transform symbol-mapping)
      transform/concretize-whitespace
      z/root
      parcera/code))
