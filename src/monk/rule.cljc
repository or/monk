(ns monk.rule
  (:require
   [rewrite-clj.zip :as z]))

(defn ns-args
  [context]
  (when (and (some-> context first :zloc z/tag (= :list))
             (some-> context first :children first z/tag (= :token))
             (some-> context first :children first z/sexpr (= 'ns))
             (some-> context first :index (> 1)))
    {:newlines 1
     :spaces 2}))

(defn ns-block-args
  [context]
  (when (and (some-> context second :zloc z/tag (= :list))
             (some-> context second :children first z/tag (= :token))
             (some-> context second :children first z/sexpr (= 'ns))
             (some-> context first :zloc z/tag (= :list))
             (some-> context first :children first z/tag (= :token))
             (some-> context first :children first z/sexpr #{:require :import :use})
             (some-> context first :index pos?))
    {:newlines 1
     :spaces 1}))

(defn map-key-values
  [context]
  (when (and (some-> context first :zloc z/tag (= :map))
             (some-> context first :index pos?))
    (if (some-> context first :index even?)
      {:newlines 1
       :spaces 1}
      {:newlines 0
       :spaces 1})))

(defn first-child
  [context]
  (when (-> context first :index zero?)
    {:newlines 0
     :spaces 0}))

(defn default
  [_context]
  {:newlines 0
   :spaces 1})
