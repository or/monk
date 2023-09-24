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

(defn- add-spaces
  [zloc context]
  (let [{:keys [newlines spaces]} (calculate-spaces zloc context)]
    (-> zloc
        (edit/insert-newlines newlines)
        (edit/insert-spaces spaces))))

(defn- transform-dispatch
  [zloc context]
  (z/tag zloc))

(defmulti transform transform-dispatch)

(defmethod transform :default
  [zloc context]
  (add-spaces zloc context))

(declare traverse-children)

(defn- handle-children
  [zloc context]
  (if (some? (z/down zloc))
    (z/up* (traverse-children (z/down* zloc) (into [{:type (z/tag zloc)}] context)))
    zloc))

(defmethod transform :list
  [zloc context]
  (-> zloc
      (add-spaces context)
      (handle-children context)))

(defmethod transform :vector
  [zloc context]
  (-> zloc
      (add-spaces context)
      (handle-children context)))

(defmethod transform :map
  [zloc context]
  (-> zloc
      (add-spaces context)
      (handle-children context)))

(defmethod transform :set
  [zloc context]
  (-> zloc
      (add-spaces context)
      (handle-children context)))

(defn- child-expression?
  [zloc]
  (z/sexpr-able? zloc))

(defn traverse-children
  [zloc context]
  (loop [zloc zloc
         context context]
    (println (apply str (repeat (count context) " ")) :traverse-children (pp zloc) #_context)
    (let [zloc (transform zloc context)
          right-form (z/right zloc)]
      (if (z/end? right-form)
        zloc
        (recur right-form (cond-> context
                            (child-expression? zloc)
                            (update-in [0 :children] (fnil conj []) (z/node zloc))))))))
