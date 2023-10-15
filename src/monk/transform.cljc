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
  [pointer base-indentation [newlines spaces]]
  (let [spaces (cond-> spaces
                 (pos? newlines) (+ base-indentation))]
    (ast/insert-newlines-and-spaces pointer newlines spaces)))

(declare transform)

(def processors
  [processor/ns-block-form
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
   processor/block-form
   #_processor/function-form
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
  [first-child context base-indentation processor]
  (loop [child first-child
         context context
         processed-children []]
    (let [effective-context (assoc context
                                   :index (util/effective-index child)
                                   :pointer child)
          [spaces new-context] (if (= child first-child)
                                 [[0 0] context]
                                 (processor effective-context))
          adjusted-child (add-spaces child base-indentation spaces)
          adjusted-child (transform adjusted-child)
          next-child (ast/right adjusted-child)]
      (if (ast/value next-child)
        (recur next-child new-context (conj processed-children [effective-context adjusted-child]))
        [(ast/up adjusted-child) processed-children]))))

(defn transform
  [pointer]
  (if (ast/value (ast/down pointer))
    (let [;; TODO: currently incremented because this is the parent list offset
          base-indentation (inc (util/get-base-indentation pointer))
          effective-index (util/effective-index pointer)
          {:keys [processor backtracker]} (pick-processor {:pointer pointer
                                                           :index effective-index})]
      (loop [pass 0
             context {}]
        (let [[adjusted-pointer processed-children] (process-children* (ast/down pointer)
                                                                       (assoc context
                                                                              :pass pass
                                                                              :base-indentation base-indentation)
                                                                       base-indentation processor)]
          (if-let [new-context (backtracker {:pointer pointer
                                             :index effective-index
                                             :pass pass
                                             :processed-children processed-children})]
            (recur (inc pass) new-context)
            adjusted-pointer))))
    pointer))
