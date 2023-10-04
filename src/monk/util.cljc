(ns monk.util
  (:require
   [rewrite-clj.zip :as z]))

(defn is-token?
  [zloc token]
  (if (set? token)
    (and (-> zloc z/tag (= :token))
         (-> zloc z/sexpr token))
    (and (-> zloc z/tag (= :token))
         (-> zloc z/sexpr (= token)))))

(defn is-list?
  [zloc]
  (some-> zloc z/tag (= :list)))

(defn is-vector?
  [zloc]
  (some-> zloc z/tag (= :vector)))

(defn is-symbol?
  [zloc]
  (and (some-> zloc z/tag (= :token))
       (symbol? (z/sexpr zloc))))

(defn is-meta?
  [zloc]
  (some-> zloc z/tag (= :meta)))

(defn- siblings-left-of
  [zloc]
  (take-while some? (iterate z/left (z/left zloc))))

(def ^:private thread-first-tokens
  #{'-> 'cond->})

(defn effective-index
  [zloc]
  (let [naive-index (count (siblings-left-of zloc))]
    (if-let [parent (z/up zloc)]
      (if (and (some-> parent z/leftmost (is-token? thread-first-tokens))
               (< 1 (effective-index parent))
               (pos? naive-index))
        (inc naive-index)
        naive-index)
      naive-index)))
