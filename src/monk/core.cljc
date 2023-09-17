(ns monk.core
  (:require
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]))

(defn process-form
  [form context]
  (println :form (z/string form))
  form)

(defn reformat-form
  [form]
  (z/root
   (loop [form (z/of-node form)
          context {}]
     (let [processed-form (process-form form context)
           next-form (z/next* processed-form)]
       (if (z/end? next-form)
         processed-form
         (recur next-form context))))))

(defn traverse
  [form context]
  (println :traverse (z/string form) context)
  (loop [form form
         context context]
    (println :traverse-loop (z/string form) context)
    (let [tag (z/tag form)
          form (if (#{:list
                      :vector
                      :map} tag)
                 (z/up* (traverse (z/down* form) (update context :path (fnil conj []) tag)))
                 form)
          next-right-form (z/right* form)]
      (if (z/end? next-right-form)
        form
        (recur next-right-form context)))))

(defn reformat-string
  [data]
  (-> data
      p/parse-string-all
      reformat-form
      n/string))

(comment

  (def form
    (p/parse-string-all "(ns foo.bar (:require [clojure.string :as str])
(:import [Foo bar]))"))

  (= (z/root form) form)

  (z/root form)
  form

  (do
    (println :start)
    (traverse form []))

  ;;
  )
