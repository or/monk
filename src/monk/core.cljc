(ns monk.core
  (:require
   [clojure.zip :as z]
   [monk.ast :as ast]
   [monk.transform :as transform]
   [parcera.core :as parcera]))

(defn format-string
  [data & {:as _options}]
  (-> (ast/parse data)
      transform/unify-metadata
      ast/zipper
      transform/transform
      transform/concretize-whitespace
      z/root
      parcera/code))
