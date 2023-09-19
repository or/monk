(ns monk.core
  (:require
   [cljfmt.core :as cljfmt]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]))

(defn pp
  [zloc]
  (str (z/tag zloc) "<" (z/string zloc) ">"))

(defn- newline-node [number-of-newlines]
  (n/newline-node (apply str (repeat number-of-newlines "\n"))))

(defn insert-newlines
  [zloc number-of-newlines]
  (if (pos? number-of-newlines)
    (z/insert-left* zloc (newline-node number-of-newlines))
    zloc))

(defn- whitespace-node [number-of-spaces]
  (n/whitespace-node (apply str (repeat number-of-spaces " "))))

(defn insert-spaces
  [zloc number-of-spaces]
  (if (pos? number-of-spaces)
    (z/insert-left* zloc (whitespace-node number-of-spaces))
    zloc))

(defn calculate-spaces
  [zloc context]
  (let [newlines 0
        spaces (if (some-> context first :children count pos?)
                 1
                 0)]
    {:newlines newlines
     :spaces spaces}))

(defn process-zloc
  [zloc context]
  (let [{:keys [newlines
                spaces]} (calculate-spaces zloc context)]
    (-> zloc
        (insert-newlines newlines)
        (insert-spaces spaces))))

(defn- child-expression?
  [zloc]
  (z/sexpr-able? zloc))

(declare traverse)

(defn traverse-children
  [zloc context process-fn depth]
  (loop [zloc zloc
         context context]
    (println (apply str (repeat depth " ")) :traverse-children (pp zloc) #_context)
    (let [zloc (traverse zloc context process-fn depth)
          right-form (z/right zloc)]
      (if (z/end? right-form)
        zloc
        (recur right-form (cond-> context
                            (child-expression? zloc)
                            (update-in [0 :children] (fnil conj []) (z/node zloc))))))))

(defn- descend?
  [zloc]
  (let [tag (z/tag zloc)]
    (and (#{:list :vector :map :set} tag)
         (some? (z/down zloc)))))

(defn traverse
  [zloc context process-fn depth]
  (println (apply str (repeat depth " ")) :traverse (pp zloc) #_context)
  (let [zloc (process-fn zloc context)]
    (if (descend? zloc)
      (z/up* (traverse-children (z/down* zloc) (into [{:type (z/tag zloc)}] context) process-fn (inc depth)))
      zloc)))

(defn traverse-form
  [form process-fn]
  (let [foo (traverse-children (z/of-node form) [{:type :root}] process-fn 0)]
    foo))

(defn remove-all-whitespace
  [form]
  (#'cljfmt/transform form #'cljfmt/edit-all #'cljfmt/clojure-whitespace? z/remove*))

(defn remove-all-newlines
  [form]
  (#'cljfmt/transform form #'cljfmt/edit-all #'cljfmt/clojure-whitespace? z/remove*))

(defn normalize-form
  [form]
  (-> form
      remove-all-whitespace
      remove-all-newlines))

(defn reformat-form
  [form]
  (z/root
   (-> form
       normalize-form
       (traverse-form process-zloc))))

(defn reformat-string
  [data]
  (-> data
      p/parse-string-all
      reformat-form
      n/string))

(comment

  (def form
    (p/parse-string-all "(ns foo.bar (:require [clojure.string :as str])
(:import [Foo bar]))

(def foo :bar)"))

  (do
    (println :start)
    (traverse-form form (fn [zloc context]
                          zloc)))
;;
  )
