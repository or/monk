(ns monk.edit
  (:require
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

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
