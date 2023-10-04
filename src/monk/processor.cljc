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

(defn- is-vector?
  [zloc]
  (some-> zloc z/tag (= :vector)))

(defn- is-symbol?
  [zloc]
  (and (some-> zloc z/tag (= :token))
       (symbol? (z/sexpr zloc))))

(defn- is-meta?
  [zloc]
  (some-> zloc z/tag (= :meta)))

(defn- siblings-left-of
  [zloc]
  (take-while some? (iterate z/left (z/left zloc))))

(defn- effective-index
  [zloc]
  (let [naive-index (count (siblings-left-of zloc))]
    (if-let [parent (z/up zloc)]
      (if (and (some-> parent z/leftmost (is-token? '->))
               (< 1 (effective-index parent)))
        (inc naive-index)
        naive-index)
      naive-index)))

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
   [(if (even? index)
      [1 1]
      [0 1])
    context]))

(defprocessor vector-form
  ([zloc]
   (-> zloc z/tag (= :vector)))

  ([context]
   [[0 1] context]))

(defprocessor ns-form
  ([zloc]
   (and (is-list? zloc)
        (is-token? (z/down zloc) 'ns)))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
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

(defprocessor let-form
  ([zloc]
   (and (is-list? zloc)
        (is-token? (z/down zloc) #{'let 'letfn})))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor let-bindings
  ([zloc]
   (and (is-vector? zloc)
        (some-> zloc z/leftmost (is-token? 'let))
        (= (effective-index zloc) 1)))

  ([{:keys [index]
     :as context}]
   [(if (even? index)
      [1 1]
      [0 1])
    context]))

(defn- letfn-binding?
  [zloc]
  (and (is-vector? zloc)
       (some-> zloc z/leftmost (is-token? 'letfn))
       (= (effective-index zloc) 1)))

(defprocessor letfn-bindings
  ([zloc]
   (letfn-binding? zloc))

  ([context]
   [[1 1] context]))

(defprocessor letfn-binding-function
  ([zloc]
   (and (is-list? zloc)
        (letfn-binding? (z/up zloc))))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor when-form
  ([zloc]
   (and (is-list? zloc)
        (is-token? (z/down zloc) #{'when 'when-not})))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor if-form
  ([zloc]
   (and (is-list? zloc)
        (is-token? (z/down zloc) #{'if 'if-not})))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor ->-form
  ([zloc]
   (and (is-list? zloc)
        (is-token? (z/down zloc) '->)))

  ([{:keys [zloc]
     :as context}]
   [(if (= (effective-index zloc) 1)
      [0 1]
      [1 2])
    context]))
