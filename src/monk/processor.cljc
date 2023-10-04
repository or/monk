(ns monk.processor
  (:require
   [monk.macro :refer [defprocessor]]
   [monk.util :as util]
   [rewrite-clj.zip :as z]))

(defprocessor default
  ([_context]
   true)

  ([context]
   [[0 1] context]))

(defprocessor map-form
  ([{:keys [zloc]}]
   (-> zloc z/tag (= :map)))

  ([{:keys [index]
     :as context}]
   [(if (even? index)
      [1 1]
      [0 1])
    context]))

(defprocessor vector-form
  ([{:keys [zloc]}]
   (-> zloc z/tag (= :vector)))

  ([context]
   [[0 1] context]))

(defprocessor ns-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) 'ns)))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor ns-block-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) #{:require :import :use})
        (some-> zloc z/leftmost (util/is-token? 'ns))))

  ([context]
   [[1 1] context]))

(defprocessor do-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) #{'do 'doall})))

  ([context]
   [[1 2] context]))

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

(defprocessor let-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) #{'let 'letfn})))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor let-bindings
  ([{:keys [zloc index]}]
   (and (util/is-vector? zloc)
        (some-> zloc z/leftmost (util/is-token? 'let))
        (= index 1)))

  ([{:keys [index]
     :as context}]
   [(if (even? index)
      [1 1]
      [0 1])
    context]))

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

(defprocessor letfn-binding-function
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (letfn-binding? {:zloc (z/up zloc)})))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor when-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) #{'when 'when-not})))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor if-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) #{'if 'if-not})))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor when-let-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) 'when-let)))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor if-let-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) 'if-let)))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor ->-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) '->)))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor ->>-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) '->>)))

  ([{:keys [index]
     :as context}]
   [(if (= index 1)
      [0 1]
      [1 2])
    context]))

(defprocessor as->-form
  ([{:keys [zloc]}]
   (and (util/is-list? zloc)
        (util/is-token? (z/down zloc) 'as->)))

  ([{:keys [index]
     :as context}]
   [(if (< index 3)
      [0 1]
      [1 2])
    context]))
