(ns monk.transform
  (:require
   [monk.edit :as edit]
   [rewrite-clj.zip :as z]))

(defn pp
  [zloc]
  (str (z/tag zloc) "<" (z/string zloc) ">"))

(defn calculate-spaces
  [zloc context]
  (let [newlines 0
        spaces (if (some-> context first :children count pos?)
                 1
                 0)]
    {:newlines newlines
     :spaces spaces}))

(defn- descend?
  [zloc]
  (let [tag (z/tag zloc)]
    (and (#{:list :vector :map :set} tag)
         (some? (z/down zloc)))))

(defn- transform-dispatch
  [zloc context])

(defmulti transform transform-dispatch)

(defmethod transform :default
  [zloc context]
  (let [{:keys [newlines
                spaces]} (calculate-spaces zloc context)]
    (-> zloc
        (edit/insert-newlines newlines)
        (edit/insert-spaces spaces))))

(declare traverse-children)

(defn traverse
  [zloc context]
  (println (apply str (repeat (count context) " ")) :traverse (pp zloc) #_context)
  (let [zloc (transform zloc context)]
    (if (descend? zloc)
      (z/up* (traverse-children (z/down* zloc) (into [{:type (z/tag zloc)}] context)))
      zloc)))

(defn- child-expression?
  [zloc]
  (z/sexpr-able? zloc))

(defn traverse-children
  [zloc context]
  (loop [zloc zloc
         context context]
    (println (apply str (repeat (count context) " ")) :traverse-children (pp zloc) #_context)
    (let [zloc (traverse zloc context)
          right-form (z/right zloc)]
      (if (z/end? right-form)
        zloc
        (recur right-form (cond-> context
                            (child-expression? zloc)
                            (update-in [0 :children] (fnil conj []) (z/node zloc))))))))
