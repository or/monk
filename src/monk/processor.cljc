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

(defn- is-symbol?
  [zloc]
  (and (some-> zloc z/tag (= :token))
       (symbol? (z/sexpr zloc))))

(defn- is-meta?
  [zloc]
  (some-> zloc z/tag (= :meta)))

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

(defprocessor defn-form
  ([zloc]
   (and (is-list? zloc)
        (is-token? (z/down zloc) #{'defn 'defn-})))

  ([{:keys [zloc seen-name?]
     :as context}]
   ;; TODO: this needs more logic for the metadata
   ;; TODO: multi arity
   (let [likely-function-name? (or (is-symbol? zloc)
                                   (is-meta? zloc))]
     [(cond
        seen-name? [1 2]
        :else [0 1])
      (cond-> context
        (and (not seen-name?)
             likely-function-name?) (assoc :seen-name? true))])))

(defprocessor def-form
  ([zloc]
   (and (is-list? zloc)
        (is-token? (z/down zloc) 'def)))

  ([{:keys [zloc seen-name?]
     :as context}]
   ;; TODO: this needs more logic for the metadata
   (let [likely-function-name? (or (is-symbol? zloc)
                                   (is-meta? zloc))]
     [(cond
        seen-name? [1 2]
        :else [0 1])
      (cond-> context
        (and (not seen-name?)
             likely-function-name?) (assoc :seen-name? true))])))
