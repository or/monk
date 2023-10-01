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
  [{:keys [zloc]}]
  (some-> zloc z/tag (= :list)))

(defn- is-map?
  [{:keys [zloc]}]
  (some-> zloc z/tag (= :map)))

(defn ns-args
  [{:keys [parent
           index]}]
  (when (and (is-list? parent)
             (some-> parent :children first (is-token? 'ns))
             (< 1 index))
    {:newlines 1
     :spaces 2}))

(defn ns-block-args
  [{:keys [parent
           index]}]
  (let [parent-parent (:parent parent)]
    (when (and (is-list? parent-parent)
               (some-> parent-parent :children first (is-token? 'ns))
               (is-list? parent)
               (some-> parent :children first (is-token? #{:require :import :use}))
               (pos? index))
      {:newlines 1
       :spaces 1})))

(defn do-args
  [{:keys [parent
           index]}]
  (when (and (is-list? parent)
             (some-> parent :children first (is-token? #{'do 'doall}))
             (pos? index))
    {:newlines 1
     :spaces 2}))

(defn map-key-values
  [{:keys [parent
           index]}]
  (when (and (is-map? parent)
             (pos? index))
    (if (even? index)
      {:newlines 1
       :spaces 1}
      {:newlines 0
       :spaces 1})))

(defn first-child
  [{:keys [index]}]
  (when (and index
             (zero? index))
    {:newlines 0
     :spaces 0}))

(defn default
  [_context]
  {:newlines 0
   :spaces 1})
