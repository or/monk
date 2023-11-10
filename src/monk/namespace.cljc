(ns monk.namespace
  (:require
   [clojure.zip :as z]
   [monk.ast :as ast]))

(defn- get-first-child
  [ast]
  (first (some->> ast
                  z/down
                  (iterate ast/right-relevant)
                  (take-while some?))))

(defn- parse-spec
  [ast]
  (loop [element (z/down ast)
         {:keys [namespace]
          :as spec} {}]
    (let [parsed-symbol (when (ast/is-symbol? element)
                          (-> element
                            z/node
                            second))
          parsed-keyword (when (ast/is-keyword? element)
                           (-> element
                             z/node
                             second
                             (subs 1)
                             keyword))

          [value element] (if parsed-keyword
                            (let [value-element (ast/right-relevant element)]
                              [(cond
                                 (ast/is-vector? value-element) (some->> value-element
                                                                         z/down
                                                                         (iterate ast/right-relevant)
                                                                         (take-while some?)
                                                                         (filter ast/is-symbol?)
                                                                         (map (comp second z/node)))

                                 (ast/is-symbol? value-element) (some-> value-element z/node second)

                                 (ast/is-keyword? value-element) (some-> value-element z/node second)

                                 :else nil)
                               value-element])
                            [nil element])
          next-element (when element
                         (ast/right-relevant element))
          new-spec (cond-> spec
                     (and parsed-symbol (not namespace)) (assoc :namespace parsed-symbol)
                     (and parsed-keyword value) (assoc-in [:options parsed-keyword] value))]
      (if next-element
        (recur next-element new-spec)
        new-spec))))

(defn- add-require-spec
  [alias-map ast]
  (let [{:keys [namespace options]} (parse-spec ast)
        {:keys [as as-alias refer]} options]
    (cond-> alias-map
      namespace (cond->
                  as (update-in [:require :aliases] merge {as namespace})

                  as-alias (update-in [:require :aliases] merge {as-alias namespace})

                  ; :refer :all is special and acts much like a :use;
                  ; conj to a list, so the namespaces are ordered in precedence, i.e. the last
                  ; one defined is the first one in the list, as it would win
                  (= refer ":all") (update-in [:require :refer-all] conj namespace)

                  (seq? refer) (update-in [:require :refer]
                                          merge
                                          (into {}
                                                (map (fn [sym]
                                                       [sym (symbol namespace sym)]))
                                                refer))))))

(defn- add-use-spec
  [alias-map ast]
  (let [{:keys [namespace options]} (parse-spec ast)
        {:keys [only exclude rename]} options]
    (cond-> alias-map
      ; conj to a list, so the namespaces are ordered in precedence, i.e. the last
      ; one defined is the first one in the list, as it would win
      namespace (update :use
                        conj
                        [namespace
                         (cond-> {}
                           only (assoc :only (set only))
                           exclude (assoc :exclude (set exclude))
                           rename (assoc :rename rename))]))))

(defn- parse-ns-block
  [alias-map ast block-fn]
  (reduce (fn [alias-map element]
            (cond-> alias-map
              (ast/is-vector? element) (block-fn element)))
          alias-map
          (some->> ast
                   z/down
                   (iterate ast/right-relevant)
                   (take-while some?))))

(defn build-alias-map
  [ast]
  (let [ns-ast (loop [ast ast]
                 (let [first-child (get-first-child ast)
                       next-ast (z/next ast)]
                   (cond
                     (and (ast/is-list? ast)
                          (ast/is-particular-symbol? first-child #{'clojure.core/ns} nil nil)) ast

                     (z/end? next-ast) nil

                     :else (recur next-ast))))]
    (reduce (fn [alias-map element]
              (if (ast/is-list? element)
                (let [first-child (get-first-child element)]
                  (cond
                    (ast/is-particular-keyword? first-child #{:require}) (parse-ns-block alias-map element add-require-spec)
                    (ast/is-particular-keyword? first-child #{:use}) (parse-ns-block alias-map element add-use-spec)
                    :else alias-map))
                alias-map))
            {}
            (some->> ns-ast
                     z/down
                     (iterate ast/right-relevant)
                     (take-while some?)))))
