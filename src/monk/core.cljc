(ns monk.core
  (:require
   [monk.transform :as transform]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]))

(defn- whitespace-or-newline?
  [zloc]
  (or (z/whitespace? zloc)
      (z/linebreak? zloc)))

(defn normalize-form
  [zloc]
  (z/root
   (loop [zloc (z/of-node zloc)]
     (if-let [next-zloc (z/find-next zloc z/next* whitespace-or-newline?)]
       (recur (z/remove* next-zloc))
       zloc))))

(defn- transform-form
  [zloc]
  (z/root (transform/transform zloc [{:type :root}])))

(defn reformat-form
  [form]
  (-> form
      normalize-form
      transform-form))

(defn reformat-string
  [data]
  (-> data
      p/parse-string-all
      reformat-form
      n/string))
