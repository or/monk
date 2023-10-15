(ns monk.util
  (:require
   [clojure.string :as str]
   [monk.ast :as ast]
   [parcera.core :as parcera]
   [rewrite-clj.zip :as z]))

(defn is-token?
  [zloc token]
  (if (or (set? token)
          (map? token))
    (and (-> zloc z/tag (= :token))
         (-> zloc z/sexpr token))
    (and (-> zloc z/tag (= :token))
         (-> zloc z/sexpr (= token)))))

(defn is-keyword?
  [pointer keywords]
  (let [value (ast/value pointer)]
    (and value
         (-> value first (= :keyword))
         (-> value second (subs 1) keyword keywords))))

(defn is-symbol?
  [pointer symbols]
  (let [value (ast/value pointer)]
    (and value
         (-> value first (= :symbol))
         (-> value second symbol symbols))))

(defn is-list?
  [pointer]
  (some-> pointer ast/value first (= :list)))

(defn is-vector?
  [zloc]
  (some-> zloc z/tag (= :vector)))

(defn is-meta?
  [zloc]
  (some-> zloc z/tag (= :meta)))

(defn- num-siblings-left-of
  [{:keys [path]}]
  (if (empty? path)
    0
    (some-> path last dec)))

(def ^:private thread-first-tokens
  #{'-> 'cond->})

(defn effective-index
  [pointer]
  (let [naive-index (num-siblings-left-of pointer)]
    (if-let [parent (ast/up pointer)]
      (if (and (some-> parent ast/leftmost (is-symbol? thread-first-tokens))
               (< 1 (effective-index parent))
               (pos? naive-index))
        (inc naive-index)
        naive-index)
      naive-index)))

(defn multiline?
  [zloc]
  (str/includes? (z/string zloc) "\n"))

(defn num-chunks
  [zloc]
  (count (str/split (z/string zloc) #"[ \n]+")))

(def includes?
  #?(:clj (fn [^String a ^String b]
            (.contains a b))
     :cljs str/includes?))

(defn get-base-indentation
  [{:keys [path ast]}]
  (if (empty? path)
    0
    ; TODO: this is a terrible hack, but will do for now
    (let [s (-> ast
                (assoc-in path [:symbol ":MONK-BASE-INDENTATION"])
                parcera/code)
          prior-code (subs s 0 (str/index-of s ":MONK-BASE-INDENTATION"))
          lines (str/split prior-code #"\n")]
      (dec (count (last lines))))))
