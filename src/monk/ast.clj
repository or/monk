(ns monk.ast
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [parcera.core :as parcera]))

(defn parse
  [^String data]
  (walk/postwalk
   (fn [data]
     (if (sequential? data)
       (vec data)
       data))
   (parcera/ast data)))

(defn whitespace-node
  [number-of-newlines number-of-spaces]
  [:whitespace {:newlines number-of-newlines
                :spaces number-of-spaces}])

(defn is-particular-keyword?
  [ast keywords]
  (and (-> ast first (= :keyword))
       (-> ast second (subs 1) keyword keywords)))

(defn is-particular-symbol?
  [ast symbols]
  (and (-> ast first (= :symbol))
       (-> ast second symbol symbols)))

(defn is-top-level?
  [ast]
  (-> ast first (= :code)))

(defn is-symbol?
  [ast]
  (-> ast first (= :symbol)))

(defn is-list?
  [ast]
  (-> ast first (= :list)))

(defn is-map?
  [ast]
  (-> ast first (= :map)))

(defn is-vector?
  [ast]
  (-> ast first (= :vector)))

(defn is-metadata?
  [ast]
  (-> ast first (= :metadata)))

(defn is-metadata-entry?
  [ast]
  (-> ast first #{:metadata_entry
                  :deprecated_metadata_entry}))

(defn is-namespaced-map?
  [ast]
  (-> ast first (= :namespaced_map)))

(defn is-whitespace?
  [ast]
  (-> ast first (= :whitespace)))

(defn is-comment?
  [ast]
  (-> ast first (= :comment)))

(defn multiline?
  [ast]
  (->> (tree-seq sequential? seq ast)
       (filter (fn [node]
                 (and (vector? node)
                      (-> node first (= :whitespace))
                      (-> node second :newlines pos?))))
       seq))

(defn num-chunks
  [ast]
  (->> (tree-seq sequential? seq ast)
       (filter (fn [node]
                 (and (vector? node)
                      (-> node first (= :whitespace)))))
       (take 3)
       count
       inc))

(defn thread-first-form?
  [ast first-child]
  (and (-> ast first (= :list))
       (-> first-child first (= :symbol))
       (-> first-child second (str/ends-with? "->"))))
