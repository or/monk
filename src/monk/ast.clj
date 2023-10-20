(ns monk.ast
  (:require
   [clojure.walk :as walk]
   [parcera.core :as parcera]))

(defn parse
  [^String data]
  (walk/postwalk
   (fn [data]
     (if (sequential? data)
       (vec data)
       data))
   (parcera/ast data)))

(defn whitespace-node
  [number-of-newlines number-of-spaces]
  [:whitespace {:newlines number-of-newlines
                :spaces number-of-spaces}])
