(ns monk.util
  (:require
   [clojure.string :as str]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn is-token?
  [zloc token]
  (if (or (set? token)
          (map? token))
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

(defn multiline?
  [zloc]
  (str/includes? (z/string zloc) "\n"))

(defn num-chunks
  [zloc]
  (count (str/split (z/string zloc) #"[ \n]+")))

(def includes?
  #?(:clj (fn [^String a ^String b]
            (.contains a b))
     :cljs str/includes?))

(def ^:private start-element
  {:meta "^"
   :meta* "#^"
   :vector "["
   :map "{"
   :list "("
   :eval "#="
   :uneval "#_"
   :fn "#("
   :set "#{"
   :deref "@"
   :reader-macro "#"
   :unquote "~"
   :var "#'"
   :quote "'"
   :syntax-quote "`"
   :unquote-splicing "~@"
   :namespaced-map "#"})

(defn- prior-line-string
  [zloc]
  (loop [zloc zloc
         worklist '()]
    (if-let [p (z/left* zloc)]
      (let [s (str (n/string (z/node p)))
            new-worklist (cons s worklist)]
        (if-not (includes? s "\n")
          (recur p new-worklist)
          (apply str new-worklist)))
      (if-let [p (z/up* zloc)]
        ;; newline cannot be introduced by start-element
        (recur p (cons (start-element (n/tag (z/node p))) worklist))
        (apply str worklist)))))

(defn- last-line-in-string
  [^String s]
  (subs s (inc (.lastIndexOf s "\n"))))

(defn get-base-indentation
  [zloc]
  (-> zloc prior-line-string last-line-in-string count))
