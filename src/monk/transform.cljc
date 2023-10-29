(ns monk.transform
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [clojure.zip :as z]
   [monk.ast :as ast]
   [monk.formatter :as formatter]))

(def formatters
  [formatter/reader-conditional-form
   formatter/ns-block-form
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

(defn form-kind
  [ast]
  (let [[tag & _rest] (ast/unpack ast)]
    (cond
      (#{:whitespace
         :comment} tag) :delimiter

      (#{:discard} tag) :ineffective

      :else :effective)))

(defn- left-without
  [ast ignored-kinds]
  (loop [ast (z/left ast)]
    (if (and ast
             (get ignored-kinds (-> ast z/node first)))
      (recur (z/left ast))
      ast)))

(defn- left-relevant
  [ast]
  (left-without ast #{:whitespace :comment}))

(defn- right-without
  [ast ignored-kinds]
  (loop [ast (z/right ast)]
    (if (and ast
             (get ignored-kinds (-> ast z/node first)))
      (recur (z/right ast))
      ast)))

(defn- right-relevant
  [ast]
  (right-without ast #{:whitespace :comment}))

(defn- inc-or-zero
  [ast]
  (if ast
    (inc ast)
    0))

(defn- effective-form?
  [ast]
  (-> ast
      form-kind
      (= :effective)))

(defn- is-exempt-form?
  [ast]
  (let [previous (left-relevant ast)]
    (and (ast/is-discard? previous)
         (some-> previous z/down (ast/is-particular-symbol? #{'no-monk '!})))))

(declare transform*)

(defn- transform-children
  [{:keys [parent
           ast]
    :as context}]
  (let [index-in-parent (:index context)
        parent-thread-first-form? (:thread-first-form? parent)
        first-child (first (->> (z/down ast)
                                (iterate right-relevant)
                                (take-while some?)
                                (filter effective-form?)))
        context (assoc context
                       :first-child first-child
                       :thread-first-form? (ast/thread-first-form? ast first-child))
        new-ast (loop [child-ast (z/down ast)
                       current-index 0
                       last-sibling nil]
                  (let [kind (form-kind child-ast)
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
                                       :effective? effective?}
                        transformed-child (:ast (transform* child-context))
                        transformed-child (z/replace transformed-child (with-meta (z/node transformed-child) child-context))
                        next-child (right-relevant transformed-child)]
                    (if next-child
                      (recur next-child
                             next-index
                             (if effective?
                               transformed-child
                               last-sibling))
                      (z/up transformed-child))))]
    (assoc context :ast new-ast)))

(defn- insert-spaces-left
  [ast newlines spaces]
  (let [left-ast (z/left ast)]
    (if (ast/is-whitespace? left-ast)
      (z/right (z/replace left-ast (ast/with-whitespace-meta (z/node left-ast) newlines spaces)))
      (z/insert-left ast (ast/whitespace-node newlines spaces)))))

(defn- insert-spaces-right
  [ast newlines spaces]
  (let [right-ast (z/right ast)]
    (if (ast/is-whitespace? right-ast)
      (z/left (z/replace right-ast (ast/with-whitespace-meta (z/node right-ast) newlines spaces)))
      (z/insert-right ast (ast/whitespace-node newlines spaces)))))

(defn- process-children
  [{:keys [ast]
    :as context}]
  (let [formatter (pick-formatter context)
        children (->> (z/down ast)
                      (iterate #(right-without % #{:whitespace}))
                      (take-while some?))
        multiline?-per-child (map ast/multiline? children)
        num-chunks-per-child (map ast/num-chunks children)
        comment-children? (some ast/is-comment? children)
        require-linebreaks? (or comment-children?
                                (some identity multiline?-per-child)
                                (< (+ (count children) 2)
                                   (apply + num-chunks-per-child)))
        extra-context {:require-linebreaks? require-linebreaks?
                       :comment-children? comment-children?
                       :multiline?-per-child multiline?-per-child
                       :num-chunks-per-child num-chunks-per-child}
        new-ast (loop [child-ast (z/down ast)
                       state {}]
                  (let [child-context (meta (z/node child-ast))
                        [new-child-ast
                         new-state] (if (not child-context)
                                      [child-ast state]
                                      (let [full-context (merge child-context extra-context)
                                            [[newlines spaces]
                                             new-state] (formatter full-context state)
                                            new-child-ast (cond-> child-ast
                                                            (or (pos? newlines)
                                                                (keyword? spaces)
                                                                (pos? spaces)) (insert-spaces-left newlines spaces))
                                            new-child-ast (z/replace new-child-ast (with-meta (z/node new-child-ast)
                                                                                              (assoc full-context :state new-state)))]

                                        [new-child-ast new-state]))
                        next-child (right-relevant new-child-ast)]
                    (if next-child
                      (recur next-child new-state)
                      (z/up new-child-ast))))]
    (assoc context :ast new-ast)))

(defn- align-comments
  [first-comment initial-whitespace-node]
  (loop [ast first-comment]
    (let [[new-ast
           done?] (cond
                    (and (= ast first-comment)
                         initial-whitespace-node) (let [{:keys [newlines spaces]} (meta initial-whitespace-node)]
                                                    [(insert-spaces-left ast (or newlines 0) (or spaces 0)) false])

                    (= ast first-comment) [ast false]

                    (ast/is-comment? ast) [(insert-spaces-left ast 1 :previous-arg) false]

                    :else [(insert-spaces-left ast 1 :previous-arg) true])
          next-ast (right-without new-ast #{:whitespace})]
      (if (and (not done?)
               next-ast)
        (recur next-ast)
        new-ast))))

(defn- align-trailing-comments
  [first-comment]
  (let [is-top-level-child? (some-> first-comment z/up ast/is-top-level?)]
    (-> first-comment
        (cond->
          is-top-level-child? (insert-spaces-left 2 0))
        (align-comments nil)
        (cond->
          (not is-top-level-child?) (insert-spaces-right 1 0)))))

(defn- process-comments
  [{:keys [ast]
    :as context}]
  (let [new-ast (loop [child (z/down ast)
                       first-comment nil]
                  (let [[new-child
                         new-first-comment] (cond
                                              (and (not first-comment)
                                                   (ast/is-comment? child)) [child child]

                                              (and first-comment
                                                   (ast/is-whitespace? child)
                                                   (some-> child
                                                           z/right
                                                           ast/is-comment?
                                                           not)) [(align-comments first-comment (z/node child)) nil]

                                              (and first-comment
                                                   (not (ast/is-comment? child))
                                                   (not (ast/is-whitespace? child))) [(align-comments first-comment (ast/whitespace-node 0 0)) nil]

                                              :else [child first-comment])
                        next-child (z/right new-child)]
                    (if next-child
                      (recur next-child new-first-comment)
                      (z/up (if new-first-comment
                              (align-trailing-comments new-first-comment)
                              new-child)))))]
    (assoc context :ast new-ast)))

(defn- combine-metadata-entries-into-map
  [entries]
  (let [new-entries (first (reduce
                            (fn [[result seen-keys] [entry-kind entry-data]]
                              (let [new-entries (cond
                                                  (ast/is-map? entry-data) (map vec (partition 2 (remove
                                                                                                  #(or (ast/is-whitespace? %)
                                                                                                       (ast/is-comment? %))
                                                                                                  (rest entry-data))))
                                                  (ast/is-symbol? entry-data) [[[:keyword ":tag"] entry-data]]
                                                  :else [[entry-data [:symbol "true"]]])]
                                (loop [[[new-value
                                         :as new-entry] & remaining-entries] new-entries
                                       result result
                                       seen-keys seen-keys]
                                  (let [effective-key [entry-kind new-value]]
                                    (cond
                                      (nil? new-value) [result seen-keys]

                                      (get seen-keys effective-key) (recur remaining-entries result seen-keys)

                                      :else (recur remaining-entries
                                                   (conj result [entry-kind new-entry])
                                                   (conj seen-keys effective-key)))))))

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
                (fn [[result seen-keys] [entry-kind key
                                         :as entry]]
                  (let [effective-key [entry-kind
                                       (if (ast/is-symbol? key)
                                         "symbol"
                                         key)]]
                    (if (get seen-keys effective-key)
                      [result seen-keys]
                      [(conj result entry) (conj seen-keys effective-key)])))
                [[] #{}]
                entries))))

(defn unify-metadata
  [{:keys [ast]
    :as context}]
  (let [node (z/node ast)
        entries (into []
                      (remove #(or (ast/is-whitespace? %)
                                   (ast/is-comment? %)))
                      (subvec node 1 (dec (count node))))
        map-entries? (some (comp ast/is-map? second) entries)
        new-node (if map-entries?
                   (vec (concat [(first node)]
                                (combine-metadata-entries-into-map entries)
                                [(last node)]))

                   (vec (concat [(first node)]
                                (dedupe-inline-metadata-entries entries)
                                [(last node)])))]
    (assoc context :ast (z/replace ast new-node))))

(defn- traversible?
  [ast]
  (and (some-> ast z/node second vector?)
       (-> ast z/down z/node)))

(defn transform*
  [{:keys [ast]
    :as context}]
  (if (is-exempt-form? ast)
    context
    (cond-> context
      (ast/is-metadata? ast) unify-metadata

      (traversible? ast) (->
                           transform-children
                           process-children
                           process-comments))))

(defn transform
  [ast]
  (:ast (transform* {:ast ast})))

(defn- count-leading-spaces
  [s]
  (->> s
       (take-while #{\space})
       count))

(defn- adjust-doc-string
  [ast column]
  (let [[_ current-string] (z/node ast)
        previous-node (some-> ast z/left z/node)
        ; if the node is preceded by a whitespace node with newlines, which should
        ; be the case in a vast majority of situations, then we know the
        ; previous column of this element, which allows more accurate
        ; calculations, if not, then we must guess
        previous-column (when (and (ast/is-whitespace? ast)
                                   (str/includes? (second previous-node) "\n"))
                          (-> previous-node
                              second
                              (str/split #"\n")
                              last
                              count))
        lines (-> current-string
                  (subs 1 (dec (count current-string)))
                  str/trim
                  (str/split #"\n"))
        offsets-and-lengths (->> lines
                                 (drop 1)
                                 (map str/trimr)
                                 (map (juxt count-leading-spaces count))
                                 (map (fn [[offset length]]
                                        [offset (- length offset)])))
        trimmed-lines (map str/trim lines)
        column-shift (if previous-column
                       (- column previous-column)
                       0)
        base-column (+ column 2)
        shifted-offsets (map (fn [[offset length
                                   :as tuple]]
                               (if (pos? length)
                                 [(+ offset column-shift) length]
                                 tuple))
                             offsets-and-lengths)
        offsets-without-base-column (map (fn [[offset length
                                               :as tuple]]
                                           (if (pos? length)
                                             [(- offset base-column) length]
                                             tuple))
                                         shifted-offsets)
        main-shift (if (seq offsets-without-base-column)
                     (apply min (map first offsets-without-base-column))
                     0)
        offsets-shifted-and-lengths (->> offsets-without-base-column
                                         (map (fn [[offset length
                                                    :as tuple]]
                                                (if (pos? length)
                                                  [(- offset main-shift) length]
                                                  tuple))))
        adjusted-offsets (map (fn [[offset length]]
                                (if (pos? length)
                                  (+ base-column offset)
                                  0))
                              offsets-shifted-and-lengths)
        new-string (str (->> (interleave trimmed-lines
                                         (map (fn [offset]
                                                (apply str "\n" (repeat offset " "))) adjusted-offsets))
                             (apply str))
                        (last trimmed-lines))]
    (z/replace ast [:string (str "\"" new-string "\"")])))

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
  [ast base-indentation arg-columns column keep-original-spacing?]
  (let [node (z/node ast)
        [ast-first & ast-rest] node
        [pre post] (get element-widths ast-first)
        new-base-indentation (+ column pre)
        new-column (+ column pre)
        children (z/down ast)
        {:keys [newlines spaces]} (meta node)
        new-keep-original-spacing? (or keep-original-spacing?
                                       (is-exempt-form? ast))
        doc-string? (some-> node meta :state :doc-string?)]
    (cond
      (ast/is-whitespace? ast) (if new-keep-original-spacing?
                                 ;; TODO: should calculate new column as well
                                 [ast column]
                                 (if newlines
                                   (let [newlines (if (and (= spaces :first-arg)
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
                                     [(z/replace ast
                                                 [:whitespace (apply str (concat (repeat newlines "\n")
                                                                                 (repeat spaces " ")))])
                                      (if (pos? newlines)
                                        spaces
                                        (+ column spaces))])
                                   ;; TODO: no metadata on a previous whitespace node
                                   #_[ast (-> ast-rest first (str/split #"\n") last count)]
                                   [(z/replace ast [:whitespace ""]) column]))

      doc-string? (let [new-ast (adjust-doc-string ast column)]
                    [new-ast (-> new-ast
                                 z/node
                                 second
                                 (str/split #"\n")
                                 last
                                 count
                                 (+ new-column))])

      (and (= (count ast-rest) 1)
           (string? (first ast-rest))) [ast (-> ast-rest
                                                first
                                                (str/split #"\n")
                                                last
                                                count
                                                (+ new-column))]

      (and (zero? pre)
           (zero? post)
           (not (get #{:code :metadata} ast-first))) [ast (+ new-column (count (first ast-rest)))]

      :else (if (z/node children)
              (loop [child children
                     current-column new-column
                     arg-columns []]
                (let [[new-child
                       new-column] (concretize-whitespace* child new-base-indentation arg-columns current-column new-keep-original-spacing?)
                      next-child (z/right new-child)]
                  (if next-child
                    (recur next-child new-column (conj arg-columns current-column))
                    [(z/up new-child) new-column])))
              [ast new-column]))))

(defn concretize-whitespace
  [ast]
  (first (concretize-whitespace* ast 0 [] 0 false)))
