(ns monk.core
  (:require
   [monk.ast :as ast]
   [monk.transform :as transform]
   [parcera.core :as parcera]))

(defn format-string
  [data & {:as _options}]
  (-> (ast/parse data)
      transform/remove-whitespace
      transform/transform
      transform/concretize-whitespace
      parcera/code))
