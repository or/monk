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

(defn loc?
  [ast]
  (not (or (nil? ast)
           (and (vector? ast)
                (some-> ast first keyword?)))))

(defn unpack
  [ast]
  (cond-> ast
    (loc? ast) z/node))

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

(defn- fully-qualified-symbol
  [s {:keys [require use]} symbol-mapping]
  (let [chunks (str/split s #"/" 2)
        [sym-ns sym-name] (if (= (count chunks) 2)
                            chunks
                            [nil (first chunks)])
        qualified-symbol (or (and sym-ns
                                  (symbol (get-in require [:aliases sym-ns] sym-ns) sym-name))

                             (get-in require [:refer sym-name])

                             (loop [[use-spec & rest] use]
                               (when use-spec
                                 (let [[use-namespace {:keys [only exclude]}] use-spec
                                       resolved-symbol (symbol use-namespace sym-name)]
                                   (cond
                                     (get only sym-name) resolved-symbol
                                     (get exclude sym-name) (recur rest)
                                     (get symbol-mapping resolved-symbol) resolved-symbol
                                     :else (recur rest)))))

                             (loop [[refer-all-namespace & rest] (:refer-all require)]
                               (when refer-all-namespace
                                 (let [resolved-symbol (symbol refer-all-namespace sym-name)]
                                   (cond
                                     (get symbol-mapping resolved-symbol) resolved-symbol
                                     :else (recur rest)))))

                             (symbol "clojure.core" sym-name))]
    (get symbol-mapping qualified-symbol qualified-symbol)))

(defn- symbol-matches?
  [s symbols ns-map symbol-mapping]
  (or (when-not (str/includes? s "/")
        (let [naive-symbol (symbol s)]
          (when (get symbols naive-symbol)
            naive-symbol)))
      (let [qualified-symbol (fully-qualified-symbol s ns-map symbol-mapping)]
        (when (get symbols qualified-symbol)
          qualified-symbol))))

(defn is-particular-symbol?
  [ast symbols ns-map symbol-mapping]
  (let [ast (unpack ast)]
    (and (-> ast first (= :symbol))
         (-> ast second (symbol-matches? symbols ns-map symbol-mapping)))))

(defn is-top-level?
  [ast]
  (-> ast unpack first (= :code)))

(defn is-symbol?
  [ast]
  (-> ast unpack first (= :symbol)))

(defn is-keyword?
  [ast]
  (-> ast unpack first (= :keyword)))

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

(defn is-string?
  [ast]
  (-> ast unpack first (= :string)))

(defn is-discard?
  [ast]
  (-> ast unpack first (= :discard)))

(defn multiline?
  [ast]
  ; TODO: this doesn't yet distinguish between whitespace nodes without newlines/spaces
  ; metadata that'll be removed and such nodes that are pre-formatted and must be honoured
  ; TODO: zipper? - tried once, was slower
  (->> (tree-seq sequential? seq (unpack ast))
       (filter (fn [node]
                 (or (and (vector? node)
                          (-> node first (= :whitespace))
                          (or (some-> node meta :newlines #{:keep-existing})
                              (some-> node meta :newlines pos?)))
                     (and (vector? node)
                          (= (count node) 2)
                          (-> node second string?)
                          (str/includes? (second node) "\n")))))
       seq))

(defn num-chunks
  [ast]
  ; TODO: this doesn't yet distinguish between whitespace nodes without newlines/spaces
  ; metadata that'll be removed and such nodes that are pre-formatted and must be honoured
  ; TODO: zipper? - tried once, was slower
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

(defn unwrap-metadata
  [ast]
  (if (is-metadata? ast)
    (if (loc? ast)
      (z/down ast)
      (last ast))
    ast))

(defn left-without
  [ast ignored-kinds]
  (loop [ast (z/left ast)]
    (if (and ast
             (get ignored-kinds (-> ast z/node first)))
      (recur (z/left ast))
      ast)))

(defn left-relevant
  [ast]
  (left-without ast #{:whitespace :comment}))

(defn right-without
  [ast ignored-kinds]
  (loop [ast (z/right ast)]
    (if (and ast
             (get ignored-kinds (-> ast z/node first)))
      (recur (z/right ast))
      ast)))

(defn right-relevant
  [ast]
  (right-without ast #{:whitespace :comment}))

(defn is-exempt-form?
  [ast]
  (let [previous (left-relevant ast)]
    (and (is-discard? previous)
         (some-> previous z/down (is-particular-symbol? #{'no-monk '!} nil nil)))))
