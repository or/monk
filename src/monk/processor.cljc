(ns monk.processor
  (:require
   [monk.macro :refer [defprocessor]]
   [monk.util :as util]
   [rewrite-clj.zip :as z]))

(def ^:private block-tokens
  {'ns 1
   'do 0
   'doall 0
   'doseq 1
   'loop 1
   'for 1
   'let 1
   'letfn 1
   'when 1
   'when-not 1
   'when-let 1
   'if 1
   'if-not 1
   'if-let 1
   '-> 1
   '->> 1
   'as-> 2})

(defprocessor default
  ([_context]
   true)

  ([context]
   [[0 1] context]))

(defn- paired-element*
  [num-args
   first-element-indentation
   {:keys [index]
    :as context}]
  [(cond
     (< index num-args) [0 1]
     (even? (- index num-args)) [1 first-element-indentation]
     :else [0 1])
   context])

(defprocessor map-form
  ([{:keys [zloc]}]
   (-> zloc z/tag (= :map)))

  ([context]
   (paired-element* 0 1 context)))

(defprocessor vector-form
  ([{:keys [zloc]}]
   (-> zloc z/tag (= :vector)))

  ([context]
   [[0 1] context]))

(defprocessor ns-block-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) #{:require :import :use})
        (some-> zloc z/leftmost (util/is-token? 'ns))))

  ([context]
   [[1 1] context]))

(defprocessor defn-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) #{'defn 'defn-})))

  ([{:keys [zloc seen-name?]
     :as context}]
   ;; TODO: this needs more logic for the metadata
   ;; TODO: multi arity
   (let [likely-function-name? (or (util/is-symbol? zloc)
                                   (util/is-meta? zloc))]
     [(cond
        seen-name? [1 2]
        :else [0 1])
      (cond-> context
        (and (not seen-name?)
             likely-function-name?) (assoc :seen-name? true))])))

(defprocessor def-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) 'def)))

  ([{:keys [zloc seen-name?]
     :as context}]
   ;; TODO: this needs more logic for the metadata
   (let [likely-function-name? (or (util/is-symbol? zloc)
                                   (util/is-meta? zloc))]
     [(cond
        seen-name? [1 2]
        :else [0 1])
      (cond-> context
        (and (not seen-name?)
             likely-function-name?) (assoc :seen-name? true))])))

(defprocessor fn-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) 'fn)))

  ([{:keys [zloc seen-args?]
     :as context}]
   ;; TODO: this needs more logic for the metadata
   ;; TODO: multi arity
   (let [likely-args? (util/is-vector? zloc)]
     [(cond
        seen-args? [1 2]
        :else [0 1])
      (cond-> context
        (and (not seen-args?)
             likely-args?) (assoc :seen-args? true))])))

(defprocessor let-like-bindings
  ([{:keys [zloc index]}]
   (or (and (util/is-vector? zloc)
            (some-> zloc z/leftmost (util/is-token? #{'let 'doseq 'loop 'for}))
            (= index 1))
       (and (util/is-vector? zloc)
            (some-> zloc z/left (util/is-token? :let))
            (some-> zloc z/up util/is-vector?)
            (= (some-> zloc z/up z/left)
               (some-> zloc z/up z/leftmost))
            (some-> zloc z/up z/left (util/is-token? 'for)))))

  ([context]
   (paired-element* 0 1 context)))

(defn- letfn-binding?
  [{:keys [zloc index]}]
  (and (util/is-vector? zloc)
       (some-> zloc z/leftmost (util/is-token? 'letfn))
       (= (or index
              (util/effective-index zloc)) 1)))

(defprocessor letfn-bindings
  ([context]
   (letfn-binding? context))

  ([context]
   [[1 1] context]))

(defn- block-form*
  [num-args {:keys [index]
             :as context}]
  [(if (<= index num-args)
     [0 1]
     [1 2])
   context])

(defprocessor letfn-binding-function
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (letfn-binding? {:zloc (z/up zloc)})))

  ([context]
   (block-form* 1 context)))

(defprocessor block-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) block-tokens)))

  ([{:keys [zloc]
     :as context}]
   (let [num-args (-> zloc z/leftmost z/sexpr block-tokens)]
     (block-form* num-args context))))

(defprocessor cond->-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) #{'cond-> 'cond->>})))

  ([context]
   (paired-element* 2 2 context)))

(defprocessor cond-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) 'cond)))

  ([context]
   (paired-element* 1 2 context)))

(defprocessor case-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) 'case)))

  ([context]
   (paired-element* 2 2 context)))
