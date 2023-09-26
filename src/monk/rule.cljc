(ns monk.rule
  (:require
   [rewrite-clj.zip :as z]))

(defn- is-token?
  [zloc token]
  (if (set? token)
    (and (-> zloc z/tag (= :token))
         (-> zloc z/sexpr token))
    (and (-> zloc z/tag (= :token))
         (-> zloc z/sexpr (= token)))))

(defn- is-list?
  [frame]
  (some-> frame :zloc z/tag (= :list)))

(defn- is-map?
  [frame]
  (some-> frame :zloc z/tag (= :map)))

(defn ns-args
  [context]
  (when (and (some-> context first is-list?)
             (some-> context first :children first (is-token? 'ns))
             (some-> context first :index (> 1)))
    {:newlines 1
     :spaces 2}))

(defn ns-block-args
  [context]
  (when (and (some-> context second is-list?)
             (some-> context second :children first (is-token? 'ns))
             (some-> context first is-list?)
             (some-> context first :children first (is-token? #{:require :import :use}))
             (some-> context first :index pos?))
    {:newlines 1
     :spaces 1}))

(defn map-key-values
  [context]
  (when (and (some-> context first is-map?)
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
