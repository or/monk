(ns monk.core
  (:require
   [monk.ast :as ast]
   [monk.transform :as transform]
   [parcera.core :as parcera]))

(defn format-string
  [data & {:as _options}]
  (-> (ast/parse data)
      ast/zipper
      transform/remove-whitespace
      transform/unify-metadata
      transform/transform
      transform/concretize-whitespace
      parcera/code))
