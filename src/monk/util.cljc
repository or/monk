(ns monk.util
  (:require
   [clojure.string :as str]
   [monk.ast :as ast]
   [parcera.core :as parcera]))

(defn is-particular-keyword?
  [pointer keywords]
  (let [value (ast/value pointer)]
    (and value
         (-> value first (= :keyword))
         (-> value second (subs 1) keyword keywords))))

(defn is-particular-symbol?
  [pointer symbols]
  (let [value (ast/value pointer)]
    (and value
         (-> value first (= :symbol))
         (-> value second symbol symbols))))

(defn is-symbol?
  [pointer]
  (let [value (ast/value pointer)]
    (and value
         (-> value first (= :symbol)))))

(defn is-list?
  [pointer]
  (some-> pointer ast/value first (= :list)))

(defn is-map?
  [pointer]
  (some-> pointer ast/value first (= :map)))

(defn is-vector?
  [pointer]
  (some-> pointer ast/value first (= :vector)))

(defn is-meta?
  [pointer]
  (some-> pointer ast/value first (= :metadata)))

(defn is-whitespace?
  [pointer]
  (some-> pointer ast/value first (= :whitespace)))

(defn- num-siblings-left-of
  [pointer]
  ;; TODO: needs work
  (loop [num-siblings 0
         pointer (ast/left pointer)]
    (let [value (ast/value pointer)]
      (cond
        (nil? value) num-siblings
        (-> value first (= :whitespace)) (recur num-siblings (ast/left pointer))
        :else (recur (inc num-siblings) (ast/left pointer))))))

(def ^:private thread-first-tokens
  #{'-> 'cond->})

(defn effective-index
  [pointer]
  (let [naive-index (num-siblings-left-of pointer)]
    (if-let [parent (ast/up pointer)]
      (if (and (some-> parent ast/leftmost (is-particular-symbol? thread-first-tokens))
               (< 1 (effective-index parent))
               (pos? naive-index))
        (inc naive-index)
        naive-index)
      naive-index)))

(def includes?
  #?(:clj (fn [^String a ^String b]
            (.contains a b))
     :cljs str/includes?))

(defn multiline?
  [pointer]
  (->> (tree-seq sequential? seq (ast/value pointer))
       (filter (fn [node]
                 (and (vector? node)
                      (-> node first (= :whitespace))
                      (-> node second :newlines pos?))))
       seq))

(defn num-chunks
  [pointer]
  (->> (tree-seq sequential? seq (ast/value pointer))
       (filter (fn [node]
                 (and (vector? node)
                      (-> node first (= :whitespace)))))
       (take 3)
       count
       inc))
