(ns monk.transform
  (:require
   [clojure.walk :as walk]
   [monk.ast :as ast]
   [monk.processor :as processor]))

(def processors
  [#_processor/ns-block-form
   #_processor/defn-form
   #_processor/def-form
   #_processor/fn-form
   #_processor/let-like-bindings
   #_processor/letfn-bindings
   #_processor/letfn-binding-function
   #_processor/map-form
   #_processor/vector-form
   #_processor/case-form
   #_processor/cond-form
   #_processor/cond->-form
   #_processor/block-form
   #_processor/function-form
   #_processor/top-level-form
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
  (get #{:code :list :vector :map :set} (first value)))

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
  (let [processor (pick-processor context)
        [tag & children] value
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
                                             :parent context
                                             :child-index child-index
                                             :child-index-effective child-index-effective
                                             :delimiter? delimiter?
                                             :effective? effective?}
                              processed-child (transform* child-context)]
                          [child-index
                           child-index-effective
                           (conj processed-children (assoc context
                                                           :processed-child-context child-context
                                                           :processed-child processed-child))]))
        [_ _ processed-children] (reduce process-child [nil nil []] children)]
    (into [tag]
          (mapcat (fn [{:keys [processed-child-context
                               processed-child]}]
                    (if (:delimiter? processed-child-context)
                      [processed-child]
                      (let [[newlines spaces] (processor processed-child-context)]
                        [(ast/whitespace-node newlines spaces)
                         processed-child]))))
          processed-children)))

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
