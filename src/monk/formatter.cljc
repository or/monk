(ns monk.formatter
  (:require
   [monk.ast :as ast]
   [monk.macro :refer [defformatter]]))

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

(defformatter default
  ([_context]
   true)

  ([{:keys [index]} state]
   [(if (zero? index)
      [0 0]
      [0 1])
    state]))

(defformatter top-level-form
  ([{:keys [ast]}]
   (ast/is-top-level? ast))

  ([{:keys [index]} state]
   [(if (zero? index)
      [0 0]
      [2 0])
    state]))

(defn- paired-element*
  [num-args
   first-element-indentation
   {:keys [index]}]
  (cond
    (zero? index) [0 0]
    (< index num-args) [0 1]
    (even? (- index num-args)) [1 first-element-indentation]
    :else [0 1]))

(defformatter map-form
  ([{:keys [ast]}]
   (ast/is-map? ast))

  ([context state]
   [(paired-element* 0 0 context)
    state]))

(defformatter vector-form
  ([{:keys [ast]}]
   (ast/is-vector? ast))

  ([{:keys [index
            require-linebreaks?]} state]
   [(cond
      (zero? index) [0 0]
      require-linebreaks? [1 0]
      :else [0 1])
    state]))

(defformatter ns-block-form
  ([{:keys [ast
            first-child
            first-sibling]}]
   (and (ast/is-list? ast)
        (ast/is-particular-keyword? first-child #{:require :import :use})
        (ast/is-particular-symbol? first-sibling #{'ns})))

  ([{:keys [index]} state]
   [(if (zero? index)
      [0 0]
      [1 1])
    state]))

(defformatter defn-form
  ([{:keys [ast
            first-child]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'defn 'defn-})))

  ([{:keys [ast
            index]}
    {:keys [seen-name?]
     :as state}]
   ;; TODO: this needs more logic for the metadata
   ;; TODO: multi arity
   (let [likely-function-name? (or (ast/is-symbol? ast)
                                   (ast/is-meta? ast))]
     [(cond
        (zero? index) [0 0]
        seen-name? [1 1]
        :else [0 1])
      (cond-> state
        (and (pos? index)
             (not seen-name?)
             likely-function-name?) (assoc :seen-name? true))])))

(defformatter def-form
  ([{:keys [ast
            first-child]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'def})))

  ([{:keys [ast index]}
    {:keys [seen-name?]
     :as state}]
   ;; TODO: this needs more logic for the metadata
   (let [likely-function-name? (or (ast/is-symbol? ast)
                                   (ast/is-meta? ast))]
     [(cond
        (zero? index) [0 0]
        seen-name? [1 1]
        :else [0 1])
      (cond-> state
        (and (pos? index)
             (not seen-name?)
             likely-function-name?) (assoc :seen-name? true))])))

(defformatter fn-form
  ([{:keys [ast
            first-child]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'fn})))

  ([{:keys [ast index]}
    {:keys [seen-args?]
     :as state}]
   ;; TODO: this needs more logic for the metadata
   ;; TODO: multi arity
   (let [likely-args? (ast/is-vector? ast)]
     [(cond
        (zero? index) [0 0]
        seen-args? [1 1]
        :else [0 1])
      (cond-> state
        (and (pos? index)
             (not seen-args?)
             likely-args?) (assoc :seen-args? true))])))

(defformatter let-like-bindings
  ([{:keys [ast
            index
            last-sibling
            first-sibling
            parent]}]
   (or (and (ast/is-vector? ast)
            (ast/is-particular-symbol? first-sibling #{'let 'doseq 'loop 'for})
            (= index 1))
       (and (ast/is-vector? ast)
            (ast/is-particular-keyword? last-sibling #{:let})
            (ast/is-vector? (:ast parent))
            (= (:index parent) 1)
            (ast/is-particular-symbol? (:first-sibling parent) #{'for}))))

  ([context state]
   [(paired-element* 0 0 context)
    state]))

(defn- letfn-binding?
  [{:keys [ast index
           first-sibling]}]
  (and (ast/is-vector? ast)
       (ast/is-particular-symbol? first-sibling #{'letfn})
       (= index 1)))

(defformatter letfn-bindings
  ([context]
   (letfn-binding? context))

  ([{:keys [index]} state]
   [(if (zero? index)
      [0 0]
      [1 0])
    state]))

(defn- block-form*
  [num-args {:keys [index]}]
  (cond
    (zero? index) [0 0]
    (<= index num-args) [0 1]
    :else [1 1]))

(defformatter letfn-binding-function
  ([{:keys [ast parent]}]
   (and (ast/is-list? ast)
        (letfn-binding? parent)))

  ([context state]
   [(block-form* 1 context)
    state]))

(defformatter block-form
  ([{:keys [ast
            first-child]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child block-tokens)))

  ([{:keys [first-sibling]
     :as context}
    state]
   (let [num-args (-> first-sibling second symbol block-tokens)]
     [(block-form* num-args context)
      state])))

(defformatter cond->-form
  ([{:keys [ast
            first-child]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'cond-> 'cond->>})))

  ([context state]
   [(paired-element* 2 1 context) state]))

(defformatter cond-form
  ([{:keys [ast
            first-child]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'cond})))

  ([context state]
   [(paired-element* 1 1 context) state]))

(defformatter case-form
  ([{:keys [ast
            first-child]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'case})))

  ([context state]
   [(paired-element* 2 1 context) state]))

(defformatter function-form
  ([{:keys [ast]}]
   (ast/is-list? ast))

  ([{:keys [index
            require-linebreaks?]}
    state]
   [(cond
      (zero? index) [0 0]
      require-linebreaks? (if (= 1 index)
                            [0 1]
                            [1 :first-arg])
      :else [0 1])
    state]))
