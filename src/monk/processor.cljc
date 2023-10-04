(ns monk.processor
  (:require
   [monk.macro :refer [defprocessor]]
   [rewrite-clj.zip :as z]))

(defn- is-token?
  [zloc token]
  (if (set? token)
    (and (-> zloc z/tag (= :token))
         (-> zloc z/sexpr token))
    (and (-> zloc z/tag (= :token))
         (-> zloc z/sexpr (= token)))))

(defn- is-list?
  [zloc]
  (some-> zloc z/tag (= :list)))

(defprocessor default
  ([_zloc]
   true)

  ([context]
   [[0 1] context]))

(defprocessor map-form
  ([zloc]
   (-> zloc z/tag (= :map)))

  ([{:keys [index]
     :as context}]
   [(cond
      (even? index) [1 1]
      :else [0 1])
    context]))

(defprocessor ns-form
  ([zloc]
   (and (is-list? zloc)
        (is-token? (z/down zloc) 'ns)))

  ([{:keys [index]
     :as context}]
   [(cond
      (= index 1) [0 1]
      :else [1 2])
    context]))

(defprocessor ns-block-form
  ([zloc]
   (and (is-list? zloc)
        (is-token? (z/down zloc) #{:require :import :use})
        (some-> zloc z/leftmost (is-token? 'ns))))

  ([context]
   [[1 1] context]))

(defprocessor do-form
  ([zloc]
   (and (is-list? zloc)
        (is-token? (z/down zloc) #{'do 'doall})))

  ([context]
   [[1 2] context]))
