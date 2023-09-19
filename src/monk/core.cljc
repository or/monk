(ns monk.core
  (:require
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]))

(defn ensure-newlines-to-left
  [zloc number-of-newlines]
  (cond-> zloc
    (pos? number-of-newlines) (z/insert-left* (n/newline-node (apply str (repeat number-of-newlines "\n"))))))

(defn pp
  [zloc]
  (str (z/tag zloc) "<" (z/string zloc) ">"))

(defn remove-whitespace-and-newlines-to-left
  [zloc]
  (loop [zloc zloc]
    (let [left-form (z/left* zloc)]
      (if (or (z/whitespace? left-form)
              (z/linebreak? left-form))
        (if (= (z/leftmost* left-form) left-form)
          (recur (z/down* (z/next* (z/remove* left-form))))
          (recur (z/remove* left-form)))
        zloc))))

(defn ensure-whitespace-to-left
  [zloc number-of-whitespaces]
  (cond-> zloc
    (pos? number-of-whitespaces) (z/insert-left* (n/whitespace-node (apply str (repeat number-of-whitespaces " "))))))

(defn remove-rightmost-child-whitespace-and-newline
  [zloc]
  (loop [zloc zloc]
    (let [right-form (some-> zloc z/down* z/rightmost*)]
      (if (or (z/whitespace? right-form)
              (z/linebreak? right-form))
        ;; TODO: this drops into the last child of the previous sibling, if it has children
        (recur (z/next* (z/remove* right-form)))
        zloc))))

(defn process-zloc
  [zloc context]
  (if (or (z/whitespace? zloc)
          (z/linebreak? zloc))
    zloc
    (-> zloc
        remove-whitespace-and-newlines-to-left
        (ensure-newlines-to-left 0)
        (as-> zloc
              (if (= (z/leftmost* zloc) zloc)
                zloc
                (ensure-whitespace-to-left zloc 1)))
        #_remove-rightmost-child-whitespace-and-newline)))

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
      (println :tc (pp zloc) (pp right-form))
      (if (z/end? right-form)
        zloc
        (recur right-form (cond-> context
                            (child-expression? zloc)
                            (update-in [0 :children] (fnil conj []) (z/node zloc))))))))

(defn- descend?
  [zloc]
  (let [tag (z/tag zloc)]
    (#{:list :vector :map :set} tag)))

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

(defn reformat-form
  [form]
  (z/root
   (traverse-form form process-zloc)))

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
