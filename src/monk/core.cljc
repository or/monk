(ns monk.core
  (:require
   [monk.ast :as ast]
   [monk.transform :as transform]
   [parcera.core :as parcera]))

(defn format-string
  [data & {:as _options}]
  (-> (ast/parse data)
      transform/move-postfix-comments
      transform/remove-whitespace
      transform/transform
      transform/concretize-whitespace
      parcera/code))
