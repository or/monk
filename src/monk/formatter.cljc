(ns monk.formatter
  (:require
   [clojure.string :as str]
   [clojure.zip :as z]
   [monk.ast :as ast]
   [monk.macro :refer [defformatter]]))

(defn- user-linebreak?
  [ast]
  (let [left-node (z/left ast)]
    (and (ast/is-whitespace? left-node)
         (some? (z/left left-node))
         (str/includes? (second (z/node left-node)) "\n"))))

(def ^:private block-tokens
  {'clojure.core/ns 1
   'clojure.core/do 0
   'clojure.core/doall 0
   'clojure.core/doseq 1
   'clojure.core/loop 1
   'clojure.core/for 1
   'clojure.core/let 1
   'clojure.core/letfn 1
   'clojure.core/when 1
   'clojure.core/when-not 1
   'clojure.core/when-let 1
   'clojure.core/if 1
   'clojure.core/if-not 1
   'clojure.core/if-let 1
   'clojure.core/-> 1
   'clojure.core/->> 1
   'clojure.core/as-> 2

   'clojure.test/deftest 1
   'clojure.test/are 2})

(defn- block-form*
  [num-args {:keys [ast index]}]
  (let [user-linebreak? (user-linebreak? ast)]
    (cond
      (zero? index) [0 0]
      (and (<= index num-args)
           user-linebreak?) [:keep-existing :first-arg]
      (<= index num-args) [0 1]
      user-linebreak? [:keep-existing 1]
      :else [1 1])))

(defformatter default
  ([_context]
   true)

  ([{:keys [index]} state]
   [(if (zero? index)
      [0 0]
      [0 1])
    state]))

(defformatter top-level-form
  ([{:keys [ast]}]
   (ast/is-top-level? ast))

  ([{:keys [index]} state]
   [(if (zero? index)
      [0 0]
      [2 0])
    state]))

(defn- paired-element*
  [num-args
   first-element-indentation
   {:keys [index]}]
  (cond
    (zero? index) [0 0]
    (< index num-args) [0 1]
    (even? (- index num-args)) [:keep-existing first-element-indentation]
    :else [0 1]))

(defformatter map-form
  ([{:keys [ast]}]
   (ast/is-map? ast))

  ([context state]
   [(paired-element* 0 0 context)
    state]))

(defformatter vector-form
  ([{:keys [ast]}]
   (ast/is-vector? ast))

  ([{:keys [ast index require-linebreaks?]} state]
   [(cond
      (zero? index) [0 0]
      require-linebreaks? [1 0]
      (user-linebreak? ast) [:keep-existing 0]
      :else [0 1])
    state]))

(defformatter ns-block-form
  ([{:keys [ast first-child first-sibling ns-map symbol-mapping]}]
   (and (ast/is-list? ast)
        (ast/is-particular-keyword? first-child #{:require :import :use})
        (ast/is-particular-symbol? first-sibling #{'clojure.core/ns} ns-map symbol-mapping)))

  ([{:keys [index]} state]
   [(if (zero? index)
      [0 0]
      [1 1])
    state]))

(defn- fn-supporting-multi-arity-form?
  [{:keys [ast first-child ns-map symbol-mapping]}]
  (and (ast/is-list? ast)
       (ast/is-particular-symbol? first-child #{'clojure.core/defn 'clojure.core/defn- 'clojure.core/fn} ns-map symbol-mapping)))

(defformatter defn-form
  ([{:keys [ast first-child ns-map symbol-mapping]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'clojure.core/defn 'clojure.core/defn-} ns-map symbol-mapping)))

  ([{:keys [ast index]}
    {:keys [seen-name? seen-args? seen-multi-arity? seen-body? doc-string-index]
     :as state}]
   ; TODO: this needs more logic for the metadata
   [(cond
      (zero? index) [0 0]
      seen-multi-arity? [2 1]
      seen-name? [1 1]
      :else [0 1])
    (cond-> (dissoc state :doc-string?)
      (and (pos? index)
           (not seen-name?)
           ; function name?
           (or (ast/is-symbol? ast)
               ; TODO: check must be smart enough to look inside
               (ast/is-metadata? ast))) (assoc :seen-name? true)

      (and (pos? index)
           seen-name?
           (not seen-multi-arity?)
           (not seen-args?)
           ; args?
           ; TODO: also needs a metadata check, but it must be smart enough to look inside
           (ast/is-vector? ast)) (assoc :seen-args? true)

      (and (pos? index)
           (not seen-args?)
           ; multi arity block?
           (ast/is-list? ast)
           (or (some-> ast z/node second ast/is-vector?)
               ; TODO: check must be smart enough to look inside
               (some-> ast z/node second ast/is-metadata?))) (assoc :seen-multi-arity? true)

      (and (pos? index)
           seen-args?
           (or doc-string-index
               (not (ast/is-string? ast)))
           (not seen-multi-arity?)
           (not seen-body?)) (assoc :seen-body? true)

      (and (pos? index)
           (not seen-args?)
           (not doc-string-index)
           (ast/is-string? ast)
           (not seen-multi-arity?)
           (not seen-body?)) (assoc :doc-string-index index
                                    :doc-string? true))]))

(defformatter defn-multi-arity-function
  ([{:keys [ast parent first-child]}]
   (and (ast/is-list? ast)
        (or (ast/is-vector? first-child)
            ; TODO: check must be smart enough to look inside
            (ast/is-metadata? first-child))
        (fn-supporting-multi-arity-form? parent)))

  ([{:keys [index]} state]
   [(if (zero? index)
      [0 0]
      [1 0])
    state]))

(defformatter def-form
  ([{:keys [ast first-child ns-map symbol-mapping]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'clojure.core/def} ns-map symbol-mapping)))

  ([{:keys [ast index]}
    {:keys [seen-name? seen-body? doc-string-index]
     :as state}]
   ; TODO: this needs more logic for the metadata
   (let [likely-var-name? (or (ast/is-symbol? ast)
                              (ast/is-metadata? ast))]
     [(cond
        (zero? index) [0 0]
        seen-name? [1 1]
        :else [0 1])
      (cond-> (dissoc state :doc-string?)
        (and (pos? index)
             (not seen-name?)
             likely-var-name?) (assoc :seen-name? true)

        (and (pos? index)
             seen-name?
             (or doc-string-index
                 (not (ast/is-string? ast)))
             (not seen-body?)) (assoc :seen-body? true)

        (and (pos? index)
             seen-name?
             (not doc-string-index)
             (ast/is-string? ast)
             (not seen-body?)) (assoc :doc-string-index index
                                      :doc-string? true))])))

(defformatter fn-form
  ([{:keys [ast first-child ns-map symbol-mapping]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'clojure.core/fn} ns-map symbol-mapping)))

  ([{:keys [ast index]}
    {:keys [seen-name? seen-args? seen-multi-arity?]
     :as state}]
   ; TODO: this needs more logic for the metadata
   (let [multi-arity-block? (and (ast/is-list? ast)
                                 (or (some-> ast z/node second ast/is-vector?)
                                     ; TODO: check must be smart enough to look inside
                                     (some-> ast z/node second ast/is-metadata?)))]
     [(cond
        (zero? index) [0 0]
        seen-multi-arity? [2 1]
        multi-arity-block? [1 1]
        seen-name? [1 1]
        seen-args? [1 1]
        :else [0 1])
      (cond-> state
        (and (pos? index)
             (not seen-name?)
             (not seen-args?)
             ; function name?
             (or (ast/is-symbol? ast)
               ; TODO: check must be smart enough to look inside
                 (ast/is-metadata? ast))) (assoc :seen-name? true)

        (and (pos? index)
             (not seen-args?)
             ; args?
             ; TODO: also needs a metadata check, but it must be smart enough to look inside
             (ast/is-vector? ast)) (assoc :seen-args? true)

        (and (pos? index)
             (not seen-args?)
             multi-arity-block?) (assoc :seen-multi-arity? true))])))

(defformatter let-like-bindings
  ([{:keys [ast index last-sibling first-sibling parent ns-map symbol-mapping]}]
   (or (and (ast/is-vector? ast)
            (ast/is-particular-symbol? first-sibling #{'clojure.core/let 'clojure.core/doseq 'clojure.core/loop 'clojure.core/for} ns-map symbol-mapping)
            (= index 1))
       (and (ast/is-vector? ast)
            (ast/is-particular-keyword? last-sibling #{:let})
            (ast/is-vector? (:ast parent))
            (= (:index parent) 1)
            (ast/is-particular-symbol? (:first-sibling parent) #{'clojure.core/for} ns-map symbol-mapping))))

  ([context state]
   [(paired-element* 0 0 context)
    state]))

(defn- letfn-binding?
  [{:keys [ast index first-sibling ns-map symbol-mapping]}]
  (and (or (ast/is-vector? ast)
           ; TODO: check must be smart enough to look inside
           (ast/is-metadata? ast))
       (ast/is-particular-symbol? first-sibling #{'clojure.core/letfn} ns-map symbol-mapping)
       (= index 1)))

(defformatter letfn-bindings
  ([context]
   (letfn-binding? context))

  ([{:keys [index]} state]
   [(if (zero? index)
      [0 0]
      [1 0])
    state]))

(defformatter letfn-binding-function
  ([{:keys [ast parent]}]
   (and (ast/is-list? ast)
        (letfn-binding? parent)))

  ([context state]
   [(block-form* 1 context)
    state]))

(defformatter block-form
  ([{:keys [ast first-child ns-map symbol-mapping]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child block-tokens ns-map symbol-mapping)))

  ([{:keys [first-sibling ns-map symbol-mapping]
     :as context}
    state]
   (let [matched-symbol (ast/is-particular-symbol? first-sibling block-tokens ns-map symbol-mapping)
         num-args (get block-tokens matched-symbol)]
     [(block-form* num-args context)
      state])))

(defformatter cond->-form
  ([{:keys [ast first-child ns-map symbol-mapping]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'clojure.core/cond-> 'clojure.core/cond->>} ns-map symbol-mapping)))

  ([context state]
   [(paired-element* 2 1 context) state]))

(defformatter cond-form
  ([{:keys [ast first-child ns-map symbol-mapping]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'clojure.core/cond} ns-map symbol-mapping)))

  ([context state]
   [(paired-element* 1 1 context) state]))

(defformatter case-form
  ([{:keys [ast first-child ns-map symbol-mapping]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'clojure.core/case} ns-map symbol-mapping)))

  ([context state]
   [(paired-element* 2 1 context) state]))

(defformatter function-form
  ([{:keys [ast]}]
   (ast/is-list? ast))

  ([{:keys [ast index require-linebreaks?]}
    state]
   [(cond
      (zero? index) [0 0]
      require-linebreaks? (if (= 1 index)
                            [0 1]
                            [1 :first-arg])
      (user-linebreak? ast) [:keep-existing :first-arg]
      :else [0 1])
    state]))

(defformatter metadata-form
  ([{:keys [ast]}]
   (ast/is-metadata? ast))

  ([{:keys [index first-sibling]} state]
   (let [require-linebreaks? (and (ast/is-metadata-entry? first-sibling)
                                  (ast/is-map? (second (z/node first-sibling))))]
     [(cond
        (zero? index) [0 0]
        (not require-linebreaks?) [0 1]
        :else [1 0])
      state])))

(defformatter namespaced-map-form
  ([{:keys [ast]}]
   (ast/is-namespaced-map? ast))

  ([_context state]
   [[0 0]
    state]))

(defformatter reader-conditional-form
  ([{:keys [ast parent]}]
   (and (ast/is-list? ast)
        (ast/is-reader-conditional? (:ast parent))))

  ([context state]
   [(paired-element* 0 0 context)
    state]))

(defformatter defprotocol-form
  ([{:keys [ast first-child ns-map symbol-mapping]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'clojure.core/defprotocol} ns-map symbol-mapping)))

  ([{:keys [ast index]}
    {:keys [seen-name? seen-body? doc-string-index]
     :as state}]
   ; TODO: this needs more logic for the metadata
   [(cond
      (zero? index) [0 0]
      seen-name? [1 1]
      :else [0 1])
    (cond-> (dissoc state :doc-string?)
      (and (pos? index)
           (not seen-name?)
           ; function name?
           (or (ast/is-symbol? ast)
               ; TODO: check must be smart enough to look inside
               (ast/is-metadata? ast))) (assoc :seen-name? true)

      (and (pos? index)
           seen-name?
           (or doc-string-index
               (not (ast/is-string? ast)))
           (not seen-body?)) (assoc :seen-body? true)

      (and (pos? index)
           seen-name?
           (not doc-string-index)
           (ast/is-string? ast)
           (not seen-body?)) (assoc :doc-string-index index
                                    :doc-string? true))]))

(defformatter deftype-form
  ([{:keys [ast first-child ns-map symbol-mapping]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'clojure.core/deftype} ns-map symbol-mapping)))

  ([{:keys [ast index]}
    {:keys [seen-args?]
     :as state}]
   ; TODO: this needs more logic for the metadata
   (let [following-method-definition? (some-> ast ast/left-relevant ast/is-list?)]
     [(cond
        (zero? index) [0 0]
        (and seen-args?
             following-method-definition?) [2 1]
        seen-args? [1 1]
        :else [0 1])
      (cond-> state
        (and (pos? index)
             ; type name?
             (or (ast/is-symbol? ast)
                 ; TODO: check must be smart enough to look inside
                 (ast/is-metadata? ast))) (assoc :seen-name? true)

        (and (pos? index)
             (not seen-args?)
             ; args?
             ; TODO: also needs a metadata check, but it must be smart enough to look inside
             (ast/is-vector? ast)) (assoc :seen-args? true))])))

(defformatter extend-protocol-form
  ([{:keys [ast first-child ns-map symbol-mapping]}]
   (and (ast/is-list? ast)
        (ast/is-particular-symbol? first-child #{'clojure.core/extend-protocol
                                                 'clojure.core/extend-type} ns-map symbol-mapping)))

  ([{:keys [ast index]}
    {:keys [seen-name?]
     :as state}]
   ; TODO: this needs more logic for the metadata
   (let [following-method-definition? (some-> ast ast/left-relevant ast/is-list?)]
     [(cond
        (zero? index) [0 0]
        (and seen-name?
             following-method-definition?) [2 1]
        seen-name? [1 1]
        :else [0 1])
      (cond-> state
        (and (pos? index)
             (not seen-name?)
             ; type name?
             (or (ast/is-symbol? ast)
                 ; TODO: check must be smart enough to look inside
                 (ast/is-metadata? ast))) (assoc :seen-name? true))])))

(defformatter protocol-method-definition
  ([{:keys [ast parent ns-map symbol-mapping]}]
   (and (ast/is-list? ast)
        (and (ast/is-list? ast)
             (ast/is-particular-symbol? (:first-child parent)
                                        #{'clojure.core/deftype
                                          'clojure.core/extend-protocol
                                          'clojure.core/extend-type} ns-map symbol-mapping))))

  ([{:keys [ast index]}
    {:keys [seen-args?]
     :as state}]
   [(cond
      (zero? index) [0 0]
      seen-args? [1 0]
      :else [0 1])
    (cond-> state
      (and (pos? index)
           (not seen-args?)
           ; args?
           ; TODO: also needs a metadata check, but it must be smart enough to look inside
           (ast/is-vector? ast)) (assoc :seen-args? true))]))
