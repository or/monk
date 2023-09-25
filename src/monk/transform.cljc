(ns monk.transform
  (:require
   [monk.edit :as edit]
   [rewrite-clj.zip :as z])
  (:import
   [rewrite_clj.node.forms FormsNode]))

(defn pp
  [zloc]
  (str (z/tag zloc) "<" (z/string zloc) ">"))

(defn calculate-spaces
  [zloc context]
  (let [newlines 0
        spaces 1]
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
  (if (instance? FormsNode zloc)
    :root
    (z/tag zloc)))

(defmulti transform transform-dispatch)

(defmethod transform :default
  [zloc context]
  zloc)

(declare traverse-children)

(defn- arrange-children
  [zloc context]
  (if-let [first-child (z/down zloc)]
    (loop [zloc first-child
           context context
           index 0]
      (let [next-child (z/right zloc)]
        (if (z/end? next-child)
          (z/up* zloc)
          (let [adjusted-child (add-spaces next-child context)]
            (recur adjusted-child context (inc index))))))
    zloc))

(defn- handle-children
  [zloc context]
  (if (some? (z/down zloc))
    (z/up* (traverse-children (z/down* zloc) (into [{:type (z/tag zloc)}] context)))
    zloc))

(defmethod transform :root
  [zloc context]
  ;; TODO: this only looks at the first node, needs to be fixed
  (transform (z/of-node zloc) context))

(defmethod transform :list
  [zloc context]
  (-> zloc
      (arrange-children context)
      (handle-children context)))

(defmethod transform :vector
  [zloc context]
  (-> zloc
      (arrange-children context)
      (handle-children context)))

(defmethod transform :map
  [zloc context]
  (-> zloc
      (arrange-children context)
      (handle-children context)))

(defmethod transform :set
  [zloc context]
  (-> zloc
      (arrange-children context)
      (handle-children context)))

(defn- child-expression?
  [zloc]
  (z/sexpr-able? zloc))

(defn traverse-children
  [zloc context]
  (loop [zloc zloc
         context context]
    (let [zloc (transform zloc context)
          right-form (z/right zloc)]
      (if (z/end? right-form)
        zloc
        (recur right-form (cond-> context
                            (child-expression? zloc)
                            (update-in [0 :children] (fnil conj []) (z/node zloc))))))))
