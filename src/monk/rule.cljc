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
  [zloc]
  (some-> zloc z/tag (= :list)))

(defn- is-vector?
  [zloc]
  (some-> zloc z/tag (= :vector)))

(defn- is-map?
  [zloc]
  (some-> zloc z/tag (= :map)))

(defn- is-string?
  [zloc]
  (and (-> zloc z/tag #{:token :multi-line})
       (-> zloc z/sexpr string?)))

(defn- siblings-left-of
  [zloc]
  (take-while some? (iterate z/left (z/left zloc))))

(defn- is-first-child-suffices?
  [p? zloc]
  (and (p? zloc)
       (not-any? p? (siblings-left-of zloc))))

(defn- index
  [zloc]
  (count (siblings-left-of zloc)))

(defn ns-args
  [zloc]
  (when (and (is-list? (z/up zloc))
             (some-> zloc z/leftmost (is-token? 'ns))
             (< 1 (index zloc)))
    {:newlines 1
     :spaces 2}))

(defn ns-block-args
  [zloc]
  (let [parent (z/up zloc)]
    (when (and (is-list? parent)
               (some-> parent z/leftmost (is-token? 'ns))
               (is-list? parent)
               (some-> zloc z/leftmost (is-token? #{:require :import :use}))
               (pos? (index zloc)))
      {:newlines 1
       :spaces 1})))

(defn do-args
  [zloc]
  (when (and (is-list? (z/up zloc))
             (some-> zloc z/leftmost (is-token? #{'do 'doall}))
             (pos? (index zloc)))
    {:newlines 1
     :spaces 2}))

(defn map-key-values
  [zloc]
  (let [idx (index zloc)]
    (when (and (is-map? (z/up zloc))
               (pos? idx))
      (if (even? idx)
        {:newlines 1
         :spaces 1}
        {:newlines 0
         :spaces 1}))))

(defn first-child
  [zloc]
  (when (zero? (index zloc))
    {:newlines 0
     :spaces 0}))

(defn default
  [_zloc]
  {:newlines 0
   :spaces 1})

(defn defn-doc-string
  [zloc]
  (when (and (is-list? (z/up zloc))
             (some-> zloc z/leftmost (is-token? #{'defn 'defn-}))
             (is-first-child-suffices? is-string? zloc))
    {:newlines 1
     :spaces 2}))

(defn defn-args-list
  [zloc]
  (when (and (is-list? (z/up zloc))
             (some-> zloc z/leftmost (is-token? #{'defn 'defn-}))
             (is-first-child-suffices? is-vector? zloc))
    {:newlines 1
     :spaces 2}))

(defn defn-body
  [zloc]
  (when (and (is-list? (z/up zloc))
             (some-> zloc z/leftmost (is-token? #{'defn 'defn-}))
             (some is-vector? (take-while some? (iterate z/left zloc))))
    {:newlines 1
     :spaces 2}))
