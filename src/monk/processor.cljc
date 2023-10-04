(ns monk.processor
  (:require
   [monk.macro :refer [defprocessor]]
   [rewrite-clj.zip :as z]))

(defprocessor
 default

 ([_zloc]
  true)

 ([{:keys [index]
    :as context}]
  [(if (zero? index)
     [0 0]
     [0 1])
   context]))

(defprocessor
 map-form

 ([zloc]
  (-> zloc z/tag (= :map)))

 ([{:keys [index]
    :as context}]
  [(cond
     (zero? index) [0 0]
     (even? index) [1 1]
     :else [0 1])
   context]))
