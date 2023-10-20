(ns monk.transform
  (:require
   [clojure.walk :as walk]
   [monk.ast :as ast]
   [monk.processor :as processor]))

(def processors
  [processor/ns-block-form
   processor/defn-form
   processor/def-form
   processor/fn-form
   processor/let-like-bindings
   processor/letfn-bindings
   processor/letfn-binding-function
   processor/map-form
   processor/vector-form
   processor/case-form
   processor/cond-form
   processor/cond->-form
   processor/block-form
   #_processor/function-form
   processor/top-level-form
   processor/default])

(defn pick-processor
  [context]
  (first (keep (fn [{:keys [detector processor]}]
                 (when (detector context)
                   processor))
               processors)))

(defn remove-whitespace
  [ast]
  (walk/postwalk
   (fn [data]
     (if (vector? data)
       (vec (remove (fn [x]
                      (and (vector? x)
                           (= :whitespace (first x))))
                    data))
       data))
   ast))

(defn- traversible?
  [{:keys [value]}]
  (get #{:code
         :list
         :vector
         :map
         :set
         :metadata}
       (first value)))

(defn form-kind
  [[tag & _rest]]
  (cond
    (#{:whitespace
       :comment} tag) :delimiter

    (#{:discard} tag) :ineffective

    :else :effective))

(defn- inc-or-zero
  [value]
  (if value
    (inc value)
    0))

(declare transform*)

(defn process-children
  [{:keys [value]
    :as context}]
  (let [[tag & children] value
        first-effective-child (first (filter (fn [child]
                                               (-> child
                                                   form-kind
                                                   (= :effective)))
                                             children))
        parent-context (assoc context :first-effective-child first-effective-child)
        processor (pick-processor parent-context)
        process-child (fn [[last-child-index
                            last-effective-child-index
                            processed-children] child-value]
                        (let [kind (form-kind child-value)
                              delimiter? (= kind :delimiter)
                              effective? (= kind :effective)
                              child-index (if delimiter?
                                            last-child-index
                                            (inc-or-zero last-child-index))
                              child-index-effective (if effective?
                                                      (inc-or-zero last-effective-child-index)
                                                      last-effective-child-index)
                              child-context {:value child-value
                                             :parent parent-context
                                             :child-index child-index
                                             :child-index-effective child-index-effective
                                             :first-effective-sibling first-effective-child
                                             :delimiter? delimiter?
                                             :effective? effective?}
                              processed-child (transform* child-context)]
                          [child-index
                           child-index-effective
                           (conj processed-children (assoc child-context :processed-value processed-child))]))
        [_ _ processed-children] (reduce process-child [nil nil []] children)]
    (first (reduce (fn [[result state]
                        {:keys [processed-value
                                delimiter?]
                         :as child-context}]
                     (if delimiter?
                       [(conj result processed-value)
                        state]
                       (let [[[newlines spaces] new-state] (processor child-context state)]
                         [(conj result
                                (ast/whitespace-node newlines spaces)
                                processed-value)
                          new-state])))
                   [[tag] {}]
                   processed-children))))

(defn transform*
  [{:keys [value]
    :as context}]
  (if (traversible? context)
    (process-children context)
    value))

(defn transform
  [ast]
  (transform* {:value ast}))

(def ^:private element-widths
  {:code [0 0]
   :list [1 1]
   :vector [1 1]
   :namespaced_map [1 0]
   :map [1 1]
   :set [2 1]
   :number [0 0]
   :whitespace [0 0]
   :comment [0 0]
   :symbol [0 0]
   :character [0 0]
   :string [0 0]
   :keyword [0 0]
   :macro_keyword [0 0]
   :regex [1 0]
   :auto_resolve [2 0]
   :metadata [0 0]
   :metadata_entry [1 0]
   :deprecated_metadata_entry [2 0]
   :quote [1 0]
   :var_quote [2 0]
   :discard [2 0]
   :tag [1 0]
   :backtick [1 0]
   :unquote [1 0]
   :unquote_splicing [2 0]
   :conditional [2 0]
   :conditional_splicing [3 0]
   :deref [1 0]
   :fn [1 0]
   :eval [2 0]})

(defn concretize-whitespace*
  [[ast-first & ast-rest] base-indentation arg-columns column]
  (let [[pre post] (get element-widths ast-first)
        new-base-indentation (+ column pre)
        new-column (+ column pre)
        [new-ast-rest new-current-column] (cond
                                            (= ast-first
                                               :whitespace) (let [{:keys [newlines spaces]} (first ast-rest)
                                                                  spaces (cond
                                                                           (= spaces :first-arg) (second arg-columns)
                                                                           (pos? newlines) (+ base-indentation spaces)
                                                                           :else spaces)]
                                                              [[(apply str (concat (repeat newlines "\n")
                                                                                   (repeat spaces " ")))]
                                                               (if (pos? newlines)
                                                                 spaces
                                                                 (+ column spaces))])
                                            (and (= (count ast-rest) 1)
                                                 (string? (first ast-rest))) [ast-rest
                                                                              (+ column (count (first ast-rest)))]
                                            (and (zero? pre)
                                                 (zero? post)
                                                 (not (get #{:code :metadata} ast-first))) [ast-rest (+ new-column (count (first ast-rest)))]
                                            :else (reduce (fn [[new-ast-rest current-column arg-columns] child]
                                                            (let [new-arg-columns (conj arg-columns (inc current-column))
                                                                  [new-child
                                                                   new-column] (concretize-whitespace* child new-base-indentation new-arg-columns current-column)]
                                                              [(conj new-ast-rest new-child)
                                                               new-column
                                                               new-arg-columns]))
                                                          [[]
                                                           new-column
                                                           []]
                                                          ast-rest))]
    [(into [ast-first] new-ast-rest)
     new-current-column]))

(defn concretize-whitespace
  [ast]
  (first (concretize-whitespace* ast 0 [] 0)))
