(ns monk.transform
  (:require
   [clojure.walk :as walk]
   [monk.ast :as ast]
   [monk.formatter :as formatter]))

(def formatters
  [formatter/ns-block-form
   formatter/defn-form
   formatter/defn-multi-arity-function
   formatter/def-form
   formatter/fn-form
   formatter/let-like-bindings
   formatter/letfn-bindings
   formatter/letfn-binding-function
   formatter/metadata-form
   formatter/map-form
   formatter/vector-form
   formatter/case-form
   formatter/cond-form
   formatter/cond->-form
   formatter/block-form
   formatter/function-form
   formatter/top-level-form
   formatter/namespaced-map-form
   formatter/default])

(defn pick-formatter
  [context]
  (first (keep (fn [{:keys [detector formatter]}]
                 (when (detector context)
                   formatter))
               formatters)))

(defn remove-whitespace
  [ast]
  (walk/postwalk
   (fn [data]
     (if (vector? data)
       (vec (remove (fn [x]
                      (and (vector? x)
                           (= :whitespace (first x))))
                    data))
       data))
   ast))

(defn form-kind
  [[tag & _rest]]
  (cond
    (#{:whitespace
       :comment} tag) :delimiter

    (#{:discard} tag) :ineffective

    :else :effective))

(defn- inc-or-zero
  [ast]
  (if ast
    (inc ast)
    0))

(declare transform*)

(defn- transform-children
  [{:keys [parent
           ast]
    :as context}]
  (let [index-in-parent (:index context)
        parent-thread-first-form? (:thread-first-form? parent)
        [_ & children] ast
        first-child (first (filter (fn [child]
                                     (-> child
                                         form-kind
                                         (= :effective)))
                                   children))
        context (assoc context
                       :first-child first-child
                       :thread-first-form? (ast/thread-first-form? ast first-child))
        transform-child (fn [[current-index
                              transformed-children
                              last-sibling] child-ast]
                          (let [kind (form-kind child-ast)
                                delimiter? (= kind :delimiter)
                                effective? (= kind :effective)
                                next-index (if effective?
                                             (let [new-index (inc-or-zero current-index)]
                                               (if (and parent-thread-first-form?
                                                        (< 1 index-in-parent)
                                                        (= new-index 1))
                                                 (inc new-index)
                                                 new-index))
                                             current-index)
                                child-context {:ast child-ast
                                               :parent context
                                               :index current-index
                                               :first-sibling first-child
                                               :last-sibling last-sibling
                                               :delimiter? delimiter?
                                               :effective? effective?}
                                transformed-child (:ast (transform* child-context))]
                            [next-index
                             (conj transformed-children (assoc child-context :transformed-ast transformed-child))
                             (if effective?
                               transformed-child
                               last-sibling)]))]
    (assoc context :transformed-children (second (reduce transform-child [0 [] nil] children)))))

(defn- process-children
  [{:keys [ast transformed-children]
    :as context}]
  (let [formatter (pick-formatter context)
        tag (first ast)
        multiline?-per-child (map (comp ast/multiline? :transformed-ast) transformed-children)
        num-chunks-per-child (map (comp ast/num-chunks :transformed-ast) transformed-children)
        comment-children? (some identity (map (comp ast/is-comment? :transformed-ast) transformed-children))
        require-linebreaks? (or comment-children?
                                (some identity multiline?-per-child)
                                (< (+ (count transformed-children) 2)
                                   (apply + num-chunks-per-child)))
        extra-context {:require-linebreaks? require-linebreaks?
                       :comment-children? comment-children?
                       :multiline?-per-child multiline?-per-child
                       :num-chunks-per-child num-chunks-per-child}
        format-child (fn [[result state]
                          {:keys [transformed-ast delimiter?]
                           :as child-context}]
                       (if delimiter?
                         [(conj result transformed-ast)
                          state]
                         (let [[[newlines spaces]
                                new-state] (formatter (merge child-context extra-context) state)]
                           [(-> result
                                (cond->
                                  (or (pos? newlines)
                                      (keyword? spaces)
                                      (pos? spaces)) (conj (ast/whitespace-node newlines spaces)))
                                (conj transformed-ast))
                            new-state])))]
    (-> context
        (dissoc :transformed-children)
        (assoc :ast (->> transformed-children
                         (reduce format-child [[] {}])
                         first
                         (into [tag]))))))

(defn- add-comments
  [children comments initial-whitespace-node]
  (let [whitespace-node (ast/whitespace-node 1 :previous-arg)]
    (-> children
        (cond->
          initial-whitespace-node (conj initial-whitespace-node))
        (into (interpose whitespace-node comments))
        (cond->
          initial-whitespace-node (conj (ast/whitespace-node 1 :previous-arg))))))

(defn- process-comments
  [{:keys [ast]
    :as context}]
  (let [[tag & children] ast
        process-comment (fn [[result comments] child]
                          (let [[tag & _] child]
                            (if (= tag :comment)
                              [result
                               (conj comments child)]
                              (if (empty? comments)
                                [(conj result child) comments]
                                (if (ast/is-whitespace? child)
                                  [(add-comments result comments child) []]
                                  [(-> result
                                       (add-comments comments nil)
                                       (conj (ast/whitespace-node 1 0))
                                       (conj child)) []])))))
        [processed-children comments] (reduce process-comment [[] []] children)
        processed-children (cond-> processed-children
                             (seq comments) (->
                                              (cond->
                                                (ast/is-top-level? ast) (conj (ast/whitespace-node 2 0)))
                                              (add-comments comments nil)
                                              (cond->
                                                (not (ast/is-top-level? ast)) (conj (ast/whitespace-node 1 0)))))]
    (assoc context :ast (into [tag] processed-children))))

(defn- traversible?
  [{:keys [ast]}]
  (some-> ast second vector?))

(defn transform*
  [context]
  (cond-> context
    (traversible? context) (->
                             transform-children
                             process-children
                             process-comments)))

(defn transform
  [ast]
  (:ast (transform* {:ast ast})))

(def ^:private element-widths
  {:code [0 0]
   :list [1 1]
   :vector [1 1]
   :namespaced_map [1 0]
   :map [1 1]
   :set [2 1]
   :number [0 0]
   :whitespace [0 0]
   :comment [0 0]
   :symbol [0 0]
   :symbolic [2 0]
   :character [0 0]
   :string [0 0]
   :keyword [0 0]
   :macro_keyword [0 0]
   :regex [1 0]
   :auto_resolve [2 0]
   :metadata [0 0]
   :metadata_entry [1 0]
   :deprecated_metadata_entry [2 0]
   :quote [1 0]
   :var_quote [2 0]
   :discard [2 0]
   :tag [1 0]
   :backtick [1 0]
   :unquote [1 0]
   :unquote_splicing [2 0]
   :conditional [2 0]
   :conditional_splicing [3 0]
   :deref [1 0]
   :fn [1 0]
   :eval [2 0]})

(defn concretize-whitespace*
  [[ast-first & ast-rest] base-indentation arg-columns column]
  (let [[pre post] (get element-widths ast-first)
        new-base-indentation (+ column pre)
        new-column (+ column pre)
        [new-ast-rest
         new-current-column] (cond
                               (= ast-first
                                  :whitespace) (let [{:keys [newlines spaces]} (first ast-rest)
                                                     newlines (if (and (= spaces :first-arg)
                                                                       (< (count arg-columns) 3))
                                                                0
                                                                newlines)
                                                     spaces (cond
                                                              (= spaces :first-arg) (if (< (count arg-columns) 3)
                                                                                      1
                                                                                      (nth arg-columns 2))
                                                              (= spaces :previous-arg) (last arg-columns)
                                                              (pos? newlines) (+ base-indentation spaces)
                                                              :else spaces)]
                                                 [[(apply str (concat (repeat newlines "\n")
                                                                      (repeat spaces " ")))]
                                                  (if (pos? newlines)
                                                    spaces
                                                    (+ column spaces))])
                               (and (= (count ast-rest) 1)
                                    (string? (first ast-rest))) [ast-rest (+ new-column (count (first ast-rest)))]
                               (and (zero? pre)
                                    (zero? post)
                                    (not (get #{:code :metadata} ast-first))) [ast-rest (+ new-column (count (first ast-rest)))]
                               :else (reduce (fn [[new-ast-rest current-column arg-columns] child]
                                               (let [[new-child
                                                      new-column] (concretize-whitespace* child new-base-indentation arg-columns current-column)]
                                                 [(conj new-ast-rest new-child)
                                                  new-column
                                                  (conj arg-columns current-column)]))
                                             [[]
                                              new-column
                                              []]
                                             ast-rest))]
    [(into [ast-first] new-ast-rest)
     new-current-column]))

(defn concretize-whitespace
  [ast]
  (first (concretize-whitespace* ast 0 [] 0)))

(defn- combine-metadata-entries-into-map
  [entries]
  (let [new-entries (first (reduce
                            (fn [[result seen-keys] [entry-kind entry-data]]
                              (let [new-entries (cond
                                                  (ast/is-map? entry-data) (map vec (partition 2 (rest entry-data)))
                                                  (ast/is-symbol? entry-data) [[[:keyword ":tag"] entry-data]]
                                                  :else [[entry-data [:symbol "true"]]])]
                                (loop [[[new-key
                                         :as new-entry] & remaining-entries] new-entries
                                       result result
                                       seen-keys seen-keys]
                                  (cond
                                    (nil? new-key) [result seen-keys]

                                    (get seen-keys new-key) (recur remaining-entries result seen-keys)

                                    :else (recur remaining-entries
                                                 (conj result [entry-kind new-entry])
                                                 (conj seen-keys new-key))))))

                            [[] #{}]
                            entries))
        sorted-entries (sort new-entries)
        {deprecated-entries :deprecated_metadata_entry
         non-deprecated-entries :metadata_entry} (group-by first sorted-entries)]
    (cond-> []
      (seq deprecated-entries) (conj [:deprecated_metadata_entry
                                      (into [:map] (apply concat (map second deprecated-entries)))])

      (seq non-deprecated-entries) (conj [:metadata_entry
                                          (into [:map] (apply concat (map second non-deprecated-entries)))]))))

(defn- dedupe-inline-metadata-entries
  [entries]
  (sort (first (reduce
                (fn [[result seen-keys] [_ key
                                         :as entry]]
                  (let [effective-key (if (ast/is-symbol? key)
                                        "symbol"
                                        key)]
                    (if (get seen-keys effective-key)
                      [result seen-keys]
                      [(conj result entry) (conj seen-keys effective-key)])))
                [[] #{}]
                entries))))

(defn unify-metadata
  [ast]
  (walk/postwalk
   (fn [data]
     (if (and (vector? data)
              (ast/is-metadata? data))
       (let [entries (subvec data 1 (dec (count data)))
             map-entries? (some (comp ast/is-map? second) entries)]
         (if map-entries?
           (vec (concat [(first data)]
                        (combine-metadata-entries-into-map entries)
                        [(last data)]))

           (vec (concat [(first data)]
                        (dedupe-inline-metadata-entries entries)
                        [(last data)]))))

       data))
   ast))
