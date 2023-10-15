(ns monk.processor
  (:require
   [monk.ast :as ast]
   [monk.macro :refer [defprocessor]]
   [monk.util :as util]))

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
   [[0 1] context])

  ([{:keys [pass processed-children]}]
   nil))

(defprocessor top-level-form
  ([{:keys [pointer]}]
   (empty? (:path pointer)))

  ([context]
   [[2 0] context])

  ([{:keys [pass processed-children]}]
   nil))

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
  ([{:keys [pointer]}]
   (util/is-map? pointer))

  ([context]
   (paired-element* 0 1 context))

  ([{:keys [pass processed-children]}]
   nil))

(defprocessor vector-form
  ([{:keys [pointer]}]
   (util/is-vector? pointer))

  ([context]
   [[0 1] context])

  ([{:keys [pass processed-children]}]
   nil))

(defprocessor ns-block-form
  ([{:keys [pointer]}]
   (and (util/is-list? pointer)
        (util/is-particular-keyword? (ast/down pointer) #{:require :import :use})
        (util/is-particular-symbol? (ast/leftmost pointer) #{'ns})))

  ([context]
   [[1 2] context])

  ([{:keys [pass processed-children]}]
   nil))

(defprocessor defn-form
  ([{:keys [pointer]}]
   (and (util/is-list? pointer)
        (util/is-particular-symbol? (ast/down pointer) #{'defn 'defn-})))

  ([{:keys [pointer seen-name?]
     :as context}]
   ;; TODO: this needs more logic for the metadata
   ;; TODO: multi arity
   (let [likely-function-name? (or (util/is-symbol? pointer)
                                   (util/is-meta? pointer))]
     [(cond
        seen-name? [1 2]
        :else [0 1])
      (cond-> context
        (and (not seen-name?)
             likely-function-name?) (assoc :seen-name? true))]))

  ([{:keys [pass processed-children]}]
   nil))

(defprocessor def-form
  ([{:keys [pointer]}]
   (and (util/is-list? pointer)
        (util/is-particular-symbol? (ast/down pointer) #{'def})))

  ([{:keys [pointer seen-name?]
     :as context}]
   ;; TODO: this needs more logic for the metadata
   (let [likely-function-name? (or (util/is-symbol? pointer)
                                   (util/is-meta? pointer))]
     [(cond
        seen-name? [1 2]
        :else [0 1])
      (cond-> context
        (and (not seen-name?)
             likely-function-name?) (assoc :seen-name? true))]))

  ([{:keys [pass processed-children]}]
   nil))

(defprocessor fn-form
  ([{:keys [pointer]}]
   (and (util/is-list? pointer)
        (util/is-particular-symbol? (ast/down pointer) #{'fn})))

  ([{:keys [pointer seen-args?]
     :as context}]
   ;; TODO: this needs more logic for the metadata
   ;; TODO: multi arity
   (let [likely-args? (util/is-vector? pointer)]
     [(cond
        seen-args? [1 2]
        :else [0 1])
      (cond-> context
        (and (not seen-args?)
             likely-args?) (assoc :seen-args? true))]))

  ([{:keys [pass processed-children]}]
   nil))

(defprocessor let-like-bindings
  ([{:keys [pointer index]}]
   (or (and (util/is-vector? pointer)
            (some-> pointer ast/leftmost (util/is-particular-symbol? #{'let 'doseq 'loop 'for}))
            (= index 1))
       (and (util/is-vector? pointer)
            (some-> pointer ast/left (util/is-particular-keyword? #{:let}))
            (some-> pointer ast/up util/is-vector?)
            (= (some-> pointer ast/up ast/left)
               (some-> pointer ast/up ast/leftmost))
            (some-> pointer ast/up ast/left (util/is-particular-symbol? #{'for})))))

  ([context]
   (paired-element* 0 1 context))

  ([{:keys [pass processed-children]}]
   nil))

(defn- letfn-binding?
  [{:keys [pointer index]}]
  (and (util/is-vector? pointer)
       (some-> pointer ast/leftmost (util/is-particular-symbol? #{'letfn}))
       (= (or index
              (util/effective-index pointer)) 1)))

(defprocessor letfn-bindings
  ([context]
   (letfn-binding? context))

  ([context]
   [[1 1] context])

  ([{:keys [pass processed-children]}]
   nil))

(defn- block-form*
  [num-args {:keys [index]
             :as context}]
  [(if (<= index num-args)
     [0 1]
     [1 2])
   context])

(defprocessor letfn-binding-function
  ([{:keys [pointer]}]
   (and (util/is-list? pointer)
        (letfn-binding? {:pointer (ast/up pointer)})))

  ([context]
   (block-form* 1 context))

  ([{:keys [pass processed-children]}]
   nil))

(defprocessor block-form
  ([{:keys [pointer]}]
   (and (util/is-list? pointer)
        (util/is-particular-symbol? (ast/down pointer) block-tokens)))

  ([{:keys [pointer
            index]
     :as context}]
   (let [num-args (-> pointer ast/leftmost ast/value second symbol block-tokens)]
     (block-form* num-args context)))

  ([{:keys [pass processed-children]}]
   nil))

(defprocessor cond->-form
  ([{:keys [pointer]}]
   (and (util/is-list? pointer)
        (util/is-particular-symbol? (ast/down pointer) #{'cond-> 'cond->>})))

  ([context]
   (paired-element* 2 2 context))

  ([{:keys [pass processed-children]}]
   nil))

(defprocessor cond-form
  ([{:keys [pointer]}]
   (and (util/is-list? pointer)
        (util/is-particular-symbol? (ast/down pointer) #{'cond})))

  ([context]
   (paired-element* 1 2 context))

  ([{:keys [pass processed-children]}]
   nil))

(defprocessor case-form
  ([{:keys [pointer]}]
   (and (util/is-list? pointer)
        (util/is-particular-symbol? (ast/down pointer) #{'case})))

  ([context]
   (paired-element* 2 2 context))

  ([{:keys [pass processed-children]}]
   nil))

(defn- backtrack-if-multiline
  [{:keys [processed-children]}]
  (loop [[[_ child-pointer] & rest] processed-children]
    (if (and child-pointer
             (util/multiline? child-pointer))
      {}
      (when (seq rest)
        (recur rest)))))

(defn- backtrack-if-many-chunks
  [{:keys [processed-children]}]
  (loop [[[_ child-pointer] & rest] processed-children
         num-children 0
         num-chunks 0]
    (when child-pointer
      (let [num-children (inc num-children)
            num-chunks (+ num-chunks (util/num-chunks child-pointer))]
        (if (< (+ num-children 1)
               num-chunks)
          {}
          (when (seq rest)
            (recur rest num-children num-chunks)))))))

(defn- backtrack-if-complex
  [{:keys [pass]
    :as context}]
  (when (zero? pass)
    (or (backtrack-if-multiline context)
        (backtrack-if-many-chunks context))))

(defn- indentation-of-first-argument
  [pointer base-indentation]
  (-> pointer
      ast/leftmost
      ast/right
      util/get-base-indentation
      (- base-indentation -1)))

(defprocessor function-form
  ([{:keys [pointer]}]
   (util/is-list? pointer))

  ([{:keys [pointer pass index base-indentation]
     :as context}]
   (cond
     (zero? pass) [[0 1] context]
     (pos? pass) (if (= 1 index)
                   [[0 1] context]
                   [[1 (indentation-of-first-argument pointer base-indentation)] context])
     :else [[1 1] context]))

  ([context]
   (backtrack-if-complex context)))
