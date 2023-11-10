(ns monk.refactor.ns-block
  (:require
   [clojure.zip :as z]
   [monk.ast :as ast]))

(def ^:private ns-block-keywords
  #{:require :import :use :load :gen-class :refer-clojure :require-macros})

(def ^:private ns-block-keyword-strings
  (into #{} (map str ns-block-keywords)))

(defn- collect-chunks
  [nodes]
  (let [index (atom 0)]
    (partition-by (fn [node]
                    (let [current @index]
                      (when-not (ast/is-comment? node)
                        (swap! index inc))
                      current))
                  nodes)))

(defn- chunk-key
  [chunk]
  (let [relevant-node (last chunk)]
    (if (or (nil? relevant-node)
            (ast/is-comment? relevant-node))
      ; must be the last chunk of comments or something we can't sort,
      ; so make sure this will always be at the end
      [100]
      (let [real-node (ast/unwrap-metadata relevant-node)]
        (cond
          (and (ast/is-keyword? real-node)
               (get ns-block-keyword-strings
                    (last real-node))) [0 (last real-node) real-node]

          (ast/is-string? real-node) [1 (last real-node) real-node]

          (ast/is-symbol? real-node) [2 (last real-node) real-node]

          (or (ast/is-list? real-node)
              (ast/is-vector? real-node)
              (ast/is-discard? real-node)) (chunk-key [(first (remove (fn [node]
                                                                        (or (ast/is-whitespace? node)
                                                                            (ast/is-comment? node)))
                                                                      (rest real-node)))])
          :else [50 "" real-node])))))

(defn refactor
  [{:keys [ast]
    :as context}]
  (let [children (some-> ast z/node rest)
        relevant-children (remove ast/is-whitespace? children)
        ; TODO: depending on the type the specs in the children can also be refactored
        type (second (first (filter ast/is-keyword? relevant-children)))
        chunks (collect-chunks relevant-children)
        processed-chunks (if (not= type ":gen-class")
                           (sort-by chunk-key chunks)
                           chunks)
        new-node (into [:list] (apply concat processed-chunks))
        new-ast (z/replace ast new-node)]
    (assoc context :ast new-ast)))

(defn refactorable?
  [{:keys [ast exempt? ns-map symbol-mapping]}]
  ; TODO: needs to be more accurate
  (and (let [first-child (z/down ast)
             first-parent-sibling (z/leftmost ast)]
         (and (ast/is-list? ast)
              (ast/is-particular-keyword? first-child ns-block-keywords)
              (ast/is-particular-symbol? first-parent-sibling #{'clojure.core/ns} ns-map symbol-mapping)))
       (not exempt?)
       (not (ast/is-exempt-form? ast))))
