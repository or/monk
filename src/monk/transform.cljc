(ns monk.transform
  (:require
   [clojure.walk :as walk]
   [monk.ast :as ast]
   [monk.processor :as processor]
   [monk.util :as util]))

(defn remove-whitespace
  [{:keys [ast]}]
  (ast/make-pointer []
                    (walk/postwalk
                     (fn [data]
                       (if (vector? data)
                         (vec (remove (fn [x]
                                        (and (vector? x)
                                             (= :whitespace (first x))))
                                      data))
                         data))
                     ast)))

(defn- add-spaces
  [pointer [newlines spaces]]
  (ast/insert-newlines-and-spaces pointer newlines spaces))

(declare transform)

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
  (first (keep (fn [{:keys [detector]
                     :as processor}]
                 (when (detector context)
                   processor))
               processors)))

(defn- process-children*
  [first-child context processor]
  (loop [child first-child
         context context
         processed-children []]
    (let [effective-context (assoc context
                                   :index (util/effective-index child)
                                   :pointer child)
          [spaces new-context] (if (= child first-child)
                                 [[0 0] context]
                                 (processor effective-context))
          adjusted-child (add-spaces child spaces)
          adjusted-child (transform adjusted-child)
          next-child (ast/right adjusted-child)]
      (if (ast/value next-child)
        (recur next-child new-context (conj processed-children [effective-context adjusted-child]))
        [(ast/up adjusted-child) processed-children]))))

(defn transform
  [pointer]
  (if (ast/value (ast/down pointer))
    (let [effective-index (util/effective-index pointer)
          {:keys [processor backtracker]} (pick-processor {:pointer pointer
                                                           :index effective-index})]
      (loop [pass 0
             context {}]
        (let [[adjusted-pointer processed-children] (process-children* (ast/down pointer)
                                                                       (assoc context :pass pass)
                                                                       processor)]
          (if-let [new-context (backtracker {:pointer pointer
                                             :index effective-index
                                             :pass pass
                                             :processed-children processed-children})]
            (recur (inc pass) new-context)
            adjusted-pointer))))
    pointer))

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
