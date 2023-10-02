(ns monk.transform
  (:require
   #?(:cljs [clojure.string :as str])
   [monk.edit :as edit]
   [monk.rule :as rule]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z])
  (:import
   [rewrite_clj.node.forms FormsNode]))

(defn pp
  [zloc]
  (str (z/tag zloc) "<" (z/string zloc) ">"))

(defn calculate-spaces
  [zloc]
  (let [apply-rules (some-fn rule/ns-args
                             rule/ns-block-args
                             rule/do-args
                             rule/defn-doc-string
                             rule/defn-args-list
                             rule/defn-body
                             rule/map-key-values
                             rule/first-child
                             rule/default)]
    (apply-rules zloc)))

(def includes?
  #?(:clj (fn [^String a ^String b] (.contains a b))
     :cljs str/includes?))

(def ^:private start-element
  {:meta "^", :meta* "#^", :vector "[", :map "{"
   :list "(", :eval "#=", :uneval "#_", :fn "#("
   :set "#{", :deref "@", :reader-macro "#", :unquote "~"
   :var "#'", :quote "'", :syntax-quote "`", :unquote-splicing "~@"
   :namespaced-map "#"})

(defn- prior-line-string [zloc]
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

(defn- last-line-in-string [^String s]
  (subs s (inc (.lastIndexOf s "\n"))))

(defn- get-base-indentation
  [zloc]
  (-> zloc prior-line-string last-line-in-string count))

(defn- add-spaces
  [zloc base-indentation {:keys [newlines spaces]}]
  (let [spaces (cond-> spaces
                 (pos? newlines) (+ base-indentation))]
    (-> zloc
        (edit/insert-newlines newlines)
        (edit/insert-spaces spaces))))

(declare transform)

(defn- process-children
  [zloc]
  (let [base-indentation (get-base-indentation zloc)]
    (if-let [first-child (z/down zloc)]
      (loop [child first-child]
        (let [adjusted-child (add-spaces child base-indentation (calculate-spaces child))
              adjusted-child (transform adjusted-child)
              next-child (z/right adjusted-child)]
          (if (z/end? next-child)
            (z/up* adjusted-child)
            (recur next-child))))
      zloc)))

(defn transform
  [zloc]
  (cond
  ;; TODO: this only looks at the first node, needs to be fixed
    (instance? FormsNode zloc) (transform (z/of-node zloc))

    (z/down zloc) (process-children zloc)

    :else zloc))
