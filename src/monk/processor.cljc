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

  ([{:keys [child-index-effective]} state]
   [(if (zero? child-index-effective)
      [0 0]
      [0 1])
    state]))

(defprocessor top-level-form
  ([{:keys [value]}]
   (-> value
       first
       (= :code)))

  ([{:keys [child-index-effective]} state]
   [(if (zero? child-index-effective)
      [0 0]
      [2 0])
    state]))

(defn- paired-element*
  [num-args
   first-element-indentation
   {:keys [child-index-effective]}]
  (cond
    (zero? child-index-effective) [0 0]
    (< child-index-effective num-args) [0 1]
    (even? (- child-index-effective num-args)) [1 first-element-indentation]
    :else [0 1]))

(defprocessor map-form
  ([{:keys [value]}]
   (util/is-map? value))

  ([context state]
   [(paired-element* 0 0 context)
    state]))

(defprocessor vector-form
  ([{:keys [value]}]
   (util/is-vector? value))

  ([{:keys [child-index-effective]} state]
   [(if (zero? child-index-effective)
      [0 0]
      [0 1])
    state]))

(defprocessor ns-block-form
  ([{:keys [value
            first-effective-child
            first-effective-sibling]}]
   (and (util/is-list? value)
        (util/is-particular-keyword? first-effective-child #{:require :import :use})
        (util/is-particular-symbol? first-effective-sibling #{'ns})))

  ([{:keys [child-index-effective]} state]
   [(if (zero? child-index-effective)
      [0 0]
      [1 1])
    state]))

(defprocessor defn-form
  ([{:keys [value
            first-effective-child]}]
   (and (util/is-list? value)
        (util/is-particular-symbol? first-effective-child #{'defn 'defn-})))

  ([{:keys [value
            child-index-effective]}
    {:keys [seen-name?]
     :as state}]
   ;; TODO: this needs more logic for the metadata
   ;; TODO: multi arity
   (let [likely-function-name? (or (util/is-symbol? value)
                                   (util/is-meta? value))]
     [(cond
        (zero? child-index-effective) [0 0]
        seen-name? [1 1]
        :else [0 1])
      (cond-> state
        (and (pos? child-index-effective)
             (not seen-name?)
             likely-function-name?) (assoc :seen-name? true))])))

(defprocessor def-form
  ([{:keys [value
            first-effective-child]}]
   (and (util/is-list? value)
        (util/is-particular-symbol? first-effective-child #{'def})))

  ([{:keys [value child-index-effective]}
    {:keys [seen-name?]
     :as state}]
   ;; TODO: this needs more logic for the metadata
   (let [likely-function-name? (or (util/is-symbol? value)
                                   (util/is-meta? value))]
     [(cond
        (zero? child-index-effective) [0 0]
        seen-name? [1 1]
        :else [0 1])
      (cond-> state
        (and (pos? child-index-effective)
             (not seen-name?)
             likely-function-name?) (assoc :seen-name? true))])))

(defprocessor fn-form
  ([{:keys [value
            first-effective-child]}]
   (and (util/is-list? value)
        (util/is-particular-symbol? first-effective-child #{'fn})))

  ([{:keys [value child-index-effective]}
    {:keys [seen-args?]
     :as state}]
   ;; TODO: this needs more logic for the metadata
   ;; TODO: multi arity
   (let [likely-args? (util/is-vector? value)]
     [(cond
        (zero? child-index-effective) [0 0]
        seen-args? [1 1]
        :else [0 1])
      (cond-> state
        (and (pos? child-index-effective)
             (not seen-args?)
             likely-args?) (assoc :seen-args? true))])))

(defprocessor let-like-bindings
  ([{:keys [value child-index-effective
            first-effective-sibling]}]
   (or (and (util/is-vector? value)
            (util/is-particular-symbol? first-effective-sibling #{'let 'doseq 'loop 'for})
            (= child-index-effective 1))
       ;; TODO: needs more context from the parent
       #_(and (util/is-vector? value)
              (-> parent "left" (util/is-particular-keyword? #{:let}))
              (some-> pointer ast/up util/is-vector?)
              (= (some-> pointer ast/up ast/left)
                 (some-> pointer ast/up ast/leftmost))
              (some-> pointer ast/up ast/left (util/is-particular-symbol? #{'for})))))

  ([context state]
   [(paired-element* 0 0 context)
    state]))

(defn- letfn-binding?
  [{:keys [value child-index-effective
           first-effective-sibling]}]
  (and (util/is-vector? value)
       (util/is-particular-symbol? first-effective-sibling #{'letfn})
       (= child-index-effective 1)))

(defprocessor letfn-bindings
  ([context]
   (letfn-binding? context))

  ([{:keys [child-index-effective]} state]
   [(if (zero? child-index-effective)
      [0 0]
      [1 0])
    state]))

(defn- block-form*
  [num-args {:keys [child-index-effective]}]
  (cond
    (zero? child-index-effective) [0 0]
    (<= child-index-effective num-args) [0 1]
    :else [1 1]))

(defprocessor letfn-binding-function
  ([{:keys [value parent]}]
   (and (util/is-list? value)
        (letfn-binding? parent)))

  ([context state]
   [(block-form* 1 context)
    state]))

(defprocessor block-form
  ([{:keys [value
            first-effective-child]}]
   (and (util/is-list? value)
        (util/is-particular-symbol? first-effective-child block-tokens)))

  ([{:keys [first-effective-sibling]
     :as context}
    state]
   (let [num-args (-> first-effective-sibling second symbol block-tokens)]
     [(block-form* num-args context)
      state])))

(defprocessor cond->-form
  ([{:keys [value
            first-effective-child]}]
   (and (util/is-list? value)
        (util/is-particular-symbol? first-effective-child #{'cond-> 'cond->>})))

  ([context state]
   [(paired-element* 2 1 context) state]))

(defprocessor cond-form
  ([{:keys [value
            first-effective-child]}]
   (and (util/is-list? value)
        (util/is-particular-symbol? first-effective-child #{'cond})))

  ([context state]
   [(paired-element* 1 1 context) state]))

(defprocessor case-form
  ([{:keys [value
            first-effective-child]}]
   (and (util/is-list? value)
        (util/is-particular-symbol? first-effective-child #{'case})))

  ([context state]
   [(paired-element* 2 1 context) state]))

;; (defn- backtrack-if-multiline
;;   [{:keys [processed-children]}]
;;   (loop [[[_ child-pointer] & rest] processed-children]
;;     (if (and child-pointer
;;              (util/multiline? child-pointer))
;;       {}
;;       (when (seq rest)
;;         (recur rest)))))

;; (defn- backtrack-if-many-chunks
;;   [{:keys [processed-children]}]
;;   (loop [[[_ child-pointer] & rest] processed-children
;;          num-children 0
;;          num-chunks 0]
;;     (when child-pointer
;;       (let [num-children (inc num-children)
;;             num-chunks (+ num-chunks (util/num-chunks child-pointer))]
;;         (if (< (+ num-children 1)
;;                num-chunks)
;;           {}
;;           (when (seq rest)
;;             (recur rest num-children num-chunks)))))))

;; (defn- backtrack-if-complex
;;   [{:keys [pass]
;;     :as context}]
;;   (when (zero? pass)
;;     (or (backtrack-if-multiline context)
;;         (backtrack-if-many-chunks context))))

;; (defprocessor function-form
;;   ([{:keys [pointer]}]
;;    (util/is-list? pointer))

;;   ([{:keys [pass index]
;;      :as context}]
;;    (cond
;;      (zero? pass) [[0 1] context]
;;      (pos? pass) (if (= 1 index)
;;                    [[0 1] context]
;;                    [[1 :first-arg] context])
;;      :else [[1 1] context]))

;;   ([context]
;;    (backtrack-if-complex context)))
