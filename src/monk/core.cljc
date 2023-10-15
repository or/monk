(ns monk.core
  (:require
   [monk.ast :as ast]
   [monk.transform :as transform]
   [parcera.core :as parcera]))

(defn reformat-string
  [data & {:as _options}]
  (-> (ast/make-pointer [] (ast/parse data))
      transform/remove-whitespace
      transform/transform
      :ast
      parcera/code))
