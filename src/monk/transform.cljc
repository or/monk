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
   processor/function-form
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
  [{:keys [ast]}]
  (get #{:code
         :list
         :vector
         :map
         :set
         :metadata}
       (first ast)))

(defn form-kind
  [[tag & _rest]]
  (cond
    (#{:whitespace
       :comment} tag) :delimiter

    (#{:discard} tag) :ineffective

    :else :effective))

(defn- inc-or-zero
  [ast]
  (if ast
    (inc ast)
    0))

(declare transform*)

(defn process-children
  [{:keys [ast parent]
    :as context}]
  (let [[tag & children] ast
        first-child (first (filter (fn [child]
                                     (-> child
                                         form-kind
                                         (= :effective)))
                                   children))
        parent-context (assoc context
                              :first-child first-child
                              :thread-first-form? (ast/thread-first-form? ast first-child))
        parent-index (:index parent-context)
        parent-thread-first-form? (:thread-first-form? parent)
        processor (pick-processor parent-context)
        process-child (fn [[previous-index
                            processed-children
                            last-sibling] child-ast]
                        (let [kind (form-kind child-ast)
                              delimiter? (= kind :delimiter)
                              effective? (= kind :effective)
                              index (if effective?
                                      (let [new-index (inc-or-zero previous-index)]
                                        (if (and parent-thread-first-form?
                                                 (< 1 parent-index)
                                                 (= new-index 1))
                                          (inc new-index)
                                          new-index))
                                      previous-index)
                              child-context {:ast child-ast
                                             :parent parent-context
                                             :index index
                                             :first-sibling first-child
                                             :last-sibling last-sibling
                                             :delimiter? delimiter?
                                             :effective? effective?}
                              processed-child (transform* child-context)]
                          [index
                           (conj processed-children (assoc child-context :processed-ast processed-child))
                           (if effective?
                             processed-child
                             last-sibling)]))
        [_ processed-children] (reduce process-child [nil [] nil] children)
        multiline?-per-child (map (comp ast/multiline? :processed-ast) processed-children)
        num-chunks-per-child (map (comp ast/num-chunks :processed-ast) processed-children)
        require-linebreaks? (or (some identity multiline?-per-child)
                                (< (+ (count processed-children) 2)
                                   (apply + num-chunks-per-child)))]
    (first (reduce (fn [[result state]
                        {:keys [processed-ast
                                delimiter?]
                         :as child-context}]
                     (if delimiter?
                       [(conj result processed-ast)
                        state]
                       (let [[[newlines spaces]
                              new-state] (processor (assoc child-context
                                                           :require-linebreaks? require-linebreaks?
                                                           :multiline?-per-child multiline?-per-child
                                                           :num-chunks-per-child num-chunks-per-child)
                                                    state)]
                         [(-> result
                              (cond->
                                (or (pos? newlines)
                                    (pos? spaces)) (conj (ast/whitespace-node newlines spaces)))
                              (conj processed-ast))
                          new-state])))
                   [[tag] {}]
                   processed-children))))

(defn transform*
  [{:keys [ast]
    :as context}]
  (if (traversible? context)
    (process-children context)
    ast))

(defn transform
  [ast]
  (transform* {:ast ast}))

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
