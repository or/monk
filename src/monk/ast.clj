(ns monk.ast
  (:require
   [clojure.walk :as walk]
   [parcera.core :as parcera]))

(defrecord ASTPointer [path ast])

(defn value
  [^ASTPointer {:keys [path ast]}]
  (if (empty? path)
    ast
    (loop [[index & rest] path
           value ast]
      (let [new-value (when (and (vector? value)
                                 (< 0 index (count value)))
                        (get value index))]
        (cond
          (not (vector? new-value)) nil
          (empty? rest) new-value
          :else (recur rest new-value))))))

(defn down
  ^ASTPointer [^ASTPointer {:keys [path ast]}]
  (ASTPointer. (conj path 1) ast))

(defn up
  ^ASTPointer [^ASTPointer {:keys [path ast]}]
  (if (empty? path)
    nil
    (ASTPointer. (pop path) ast)))

(defn left
  ^ASTPointer [^ASTPointer {:keys [path ast]}]
  (if (empty? path)
    nil
    (let [parent-path (pop path)
          parent (value (ASTPointer. parent-path ast))]
      (loop [index (dec (last path))]
        (if (and (<= 1 index)
                 (some-> (get parent index) first (= :whitespace)))
          (recur (dec index))
          (ASTPointer. (conj parent-path index) ast))))))

(defn leftmost
  ^ASTPointer [^ASTPointer {:keys [path ast]}]
  (if (empty? path)
    nil
    (ASTPointer. (conj (pop path) 1) ast)))

(defn right
  ^ASTPointer [^ASTPointer {:keys [path ast]}]
  (if (empty? path)
    nil
    (let [parent-path (pop path)
          parent (value (ASTPointer. parent-path ast))]
      (loop [index (inc (last path))]
        (if (and (< index (count parent))
                 (some-> (get parent index) first (= :whitespace)))
          (recur (inc index))
          (ASTPointer. (conj parent-path index) ast))))))

(defn parse
  ^ASTPointer [^String data]
  (walk/postwalk
   (fn [data]
     (if (sequential? data)
       (vec data)
       data))
   (parcera/ast data)))

(defn make-pointer
  [path ast]
  (ASTPointer. path ast))

(defn insert-element
  "Add element in vector by index."
  [v pos el]
  (vec (concat (subvec v 0 pos)
               [el]
               (subvec v pos))))

(defn insert-left
  [^ASTPointer {:keys [path ast]} node]
  (let [index (last path)
        parent-path (pop path)
        new-ast (if (empty? parent-path)
                  (insert-element ast index node)
                  (update-in ast parent-path insert-element index node))]
    (right (ASTPointer. path new-ast))))

(defn whitespace-node
  [number-of-newlines number-of-spaces]
  [:whitespace (apply str (concat (repeat number-of-newlines "\n")
                                  (repeat number-of-spaces " ")))])

(defn insert-newlines-and-spaces
  [^ASTPointer pointer
   number-of-newlines
   number-of-spaces]
  (if (or (pos? number-of-newlines)
          (pos? number-of-spaces))
    (insert-left pointer (whitespace-node number-of-newlines number-of-spaces))
    pointer))
