(ns monk.ast
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [clojure.zip :as z]
   [parcera.core :as parcera]))

(defn zipper
  [ast]
  (z/zipper vector?
            rest
            (fn [n c]
              (with-meta (into [(first n)] c) (meta n)))
            ast))

(defn parse
  [^String data]
  (->> data
       parcera/ast
       (walk/postwalk
        (fn [data]
          (if (sequential? data)
            (vec data)
            data)))))

(defn unpack
  [ast]
  (if (or (nil? ast)
          (and (vector? ast)
               (some-> ast first keyword?)))
    ast
    (z/node ast)))

(defn with-whitespace-meta
  [node newlines spaces]
  (with-meta node
             {:newlines newlines
              :spaces spaces}))

(defn whitespace-node
  [newlines spaces]
  (with-whitespace-meta [:whitespace ""] newlines spaces))

(defn is-particular-keyword?
  [ast keywords]
  (let [ast (unpack ast)]
    (and (-> ast first (= :keyword))
         (-> ast second (subs 1) keyword keywords))))

(defn is-particular-symbol?
  [ast symbols]
  (let [ast (unpack ast)]
    (and (-> ast first (= :symbol))
         (-> ast second symbol symbols))))

(defn is-top-level?
  [ast]
  (-> ast unpack first (= :code)))

(defn is-symbol?
  [ast]
  (-> ast unpack first (= :symbol)))

(defn is-list?
  [ast]
  (-> ast unpack first (= :list)))

(defn is-map?
  [ast]
  (-> ast unpack first (= :map)))

(defn is-vector?
  [ast]
  (-> ast unpack first (= :vector)))

(defn is-metadata?
  [ast]
  (-> ast unpack first (= :metadata)))

(defn is-metadata-entry?
  [ast]
  (-> ast unpack first #{:metadata_entry
                         :deprecated_metadata_entry}))

(defn is-reader-conditional?
  [ast]
  (-> ast unpack first #{:conditional
                         :conditional_splicing}))

(defn is-namespaced-map?
  [ast]
  (-> ast unpack first (= :namespaced_map)))

(defn is-whitespace?
  [ast]
  (-> ast unpack first (= :whitespace)))

(defn is-comment?
  [ast]
  (-> ast unpack first (= :comment)))

(defn is-discard?
  [ast]
  (-> ast unpack first (= :discard)))

(defn multiline?
  [ast]
  ; TODO: zipper
  (->> (tree-seq sequential? seq (unpack ast))
       (filter (fn [node]
                 (and (vector? node)
                      (-> node first (= :whitespace))
                      (some-> node meta :newlines pos?))))
       seq))

(defn num-chunks
  [ast]
  ; TODO: zipper
  (->> (tree-seq sequential? seq (unpack ast))
       (filter (fn [node]
                 (and (vector? node)
                      (-> node first (= :whitespace)))))
       (take 3)
       count
       inc))

(defn is-first-threading-function?
  [ast]
  (let [ast (unpack ast)]
    (and (-> ast first (= :symbol))
         (-> ast second (str/ends-with? "->")))))

(defn thread-first-form?
  [ast first-child]
  (and (-> ast unpack first (= :list))
       (is-first-threading-function? (unpack first-child))))
