(ns monk.core-test
  (:require
   [clojure.test :refer [are deftest]]
   [monk.core :as sut]
   [monk.test-util :refer [prepare-str]]))

(deftest debug-format-string
  (are [input]
       (= (sut/format-string (prepare-str input) {})
          (prepare-str input))

    ""))

(deftest format-string
  (are [input]
       (= (sut/format-string (prepare-str input) {})
          (prepare-str input))

    ; ns
    "(ns foo.bar
    |  (:require
    |   [clojure.string :as str]))"

    "(ns foo.bar
    |  (:import
    |   [some.namespace Class1 Class2]))"

    "(ns foo.bar
    |  (:require
    |   [some.thing :as thing]
    |   #?(:clj [clj.some.other :as other]
    |      :cljs [cljs.some.other :as other])
    |   #?@(:clj #_! [[clj.some.other :as other]
    |                 [clj.some.other2 :as other2]]
    |       :cljs [[cljs.some.other :as other]])))"

    "(ns monk.main
    |  (:require
    |   [clojure.java.io :as io])
    |  (:gen-class))"

    ; map
    "{:key :value
    | :another-key (another-value arg1 arg2)}"

    ; vector
    "[1 2 3 4]"

    "[1 2 3 4
    | 5 6
    | 7]"

    ; do
    "(do
    |  (one-thing)
    |  (and-another-thing))"

    "(doall
    |  (one-thing))"

    ; defn
    "(defn function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2)
    |  (more-stuff arg3))"

    "(defn function-name
    |  \"Some doc string
    |
    |    with
    |    multiple lines.\"
    |  [arg1 arg2]
    |  (body))"

    "(defn- function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2)
    |  (more-stuff arg3))"

    "(defn- function-name
    |  \"Some doc string
    |
    |       with
    |
    |    multiple lines.\"
    |  [arg1 arg2]
    |  (body))"

    "(defn ^:private ^String function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2)
    |  (more-stuff arg3))"

    "(defn ^{:private true
    |        :tag String}
    |      function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2)
    |  (more-stuff arg3))"

    "(defn function-name
    |  \"Some doc string\"
    |  [arg1
    |   arg2
    |   {:keys [foo bar]
    |    :as multi-line-destructuring-or-long}]
    |  (body))"

    "(defn function-name
    |  ([arg1]
    |   (body))
    |
    |  (^String [arg1 arg2]
    |   (body))
    |
    |  ([arg1 arg2 arg3]
    |   (body)))"

    ; fn
    "(fn [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2)
    |  (more-stuff arg3))"

    "(fn function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2)
    |  (more-stuff arg3))"

    "(fn
    |  ([arg1]
    |   (body))
    |
    |  (^String [arg1 arg2]
    |   (body))
    |
    |  ([arg1 arg2 arg3]
    |   (body)))"

    ; def
    "(def var-name
    |  (some-stuff arg1 arg2))"

    "(def ^:private var-name
    |  (some-stuff arg1 arg2))"

    "(def ^:private var-name
    |  \"A doc-string.\"
    |  (some-stuff arg1 arg2))"

    ; top level
    "(ns foobar)
    |
    |(defn function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2))
    |
    |(defn function-name-2
    |  [arg1 arg2 arg3]
    |  (some-other-stuff arg1 arg3))"

    ; let
    "(let [foo :bar]
    |  (some-stuff foo)
    |  (more-stuff))"

    "(let [foo :bar
    |      another :thing]
    |  (some-stuff foo)
    |  (more-stuff another))"

    ; doseq
    "(doseq [index (range 100)]
    |  (some-stuff index)
    |  (more-stuff))"

    ; loop
    "(loop [foo :bar
    |       another :thing]
    |  (some-stuff foo)
    |  (recur 2 3))"

    ; for
    "(for [x (range 100)
    |      y (range 100)
    |      :let [z (* x y)]
    |      :when (even? z)]
    |  [x y])"

    "(for [x (range 100)
    |      y (range 100)
    |      :let [z (* x y)
    |            foobar (inc z)]
    |      :when (even? foobar)]
    |  [x y])"

    ; letfn
    "(letfn [(function-name [arg1 arg2]
    |          (do-something))]
    |  (function-name 1 2 3)
    |  (more-stuff))"

    "(letfn [(function-name [arg1 arg2]
    |          (do-something arg1 arg2)
    |          (do-other-things))
    |        (function-name-2 [arg1]
    |          (do-something-else arg1))]
    |  (function-name 1 2 3)
    |  (function-name-2 5 6))"

    ; when
    "(when (some-check 1 2)
    |  5)"

    "(when (some-check 1 2)
    |  (function-name 1 2 3)
    |  (more-stuff))"

    ; when-not
    "(when-not (some-check 1 2)
    |  5)"

    "(when-not (some-check 1 2)
    |  (function-name 1 2 3)
    |  (more-stuff))"

    ; if-not
    "(if-not (some-check 1 2)
    |  5
    |  6)"

    "(if-not (some-check 1 2)
    |  (function-name 1 2 3)
    |  (more-stuff))"

    ; when-let
    "(when-let [value (some-check 1 2)]
    |  value)"

    "(when-let [value (some-check 1 2)]
    |  (function-name 1 2 3)
    |  (more-stuff value))"

    ; if-let
    "(if-let [value (some-check 1 2)]
    |  value
    |  6)"

    "(if-let [value (some-check 1 2)]
    |  (function-name 1 2 3)
    |  (more-stuff value))"

    ;; ; ->
    ;; ; TODO: inline form, if newlines are not needed
    ;; #_"(-> some-map :key1 :key2 :key3 (assoc :key :value))"

    "(-> some-map
    |  (assoc :key :value)
    |  (assoc :another :value))"

    "(-> (-> some-map
    |      (assoc :key :value)
    |      (assoc :another :value))
    |  (assoc :key :whatever))"

    "(-> some-map
    |  (->
    |    (assoc :key :value)
    |    (assoc :another :value))
    |  (assoc :key :whatever))"

    "(-> some-map
    |  (assoc :key :whatever)
    |  (->
    |    (assoc :key :value)
    |    (assoc :another :value))
    |  (assoc :key :whatever))"

    ; ->>
    "(->> some-list
    |  (map :key)
    |  (filter identity)
    |  count)"

    "(-> some-list
    |  (conj :whatever)
    |  (->>
    |    (map :key)
    |    (filter identity))
    |  count)"

    ; as->
    "(as-> some-map name
    |  (assoc :key name)
    |  (assoc :another name))"

    ; TODO: line break after the first argument if necessary
    #_"(as-> (-> some-map
    |        (assoc :key :value)
    |        (assoc :another :value))
    |      name
    |  (assoc :key name))"

    "(-> some-map
    |  (as-> name
    |    (assoc :key name)
    |    (assoc :another name))
    |  (assoc :key :whatever))"

    "(-> some-map
    |  (assoc :key :whatever)
    |  (as-> name
    |    (assoc :key name)
    |    (assoc :another name))
    |  (assoc :key :whatever))"

    "(-> some-map
    |  (as-> name
    |    (some-function arg1
    |                   (do
    |                     (some-stuff))
    |                   arg3)
    |    (assoc :another name))
    |  (assoc :key :whatever))"

    ; cond->
    "(cond-> some-map
    |  something-truthy? (assoc :key :whatever)
    |  another? (assoc :key :whatever))"

    ; cond
    "(cond
    |  something-truthy? :whatever
    |  another? :something-else
    |  :else (calculate-default))"

    ; case
    "(case value
    |  value (assoc :key :whatever)
    |  another-value (assoc :key :whatever)
    |  default-value)"

    ; function call
    "(some-function arg1 arg2 arg3)"

    "(some-function arg1 arg2
    |
    |               arg3)"

    "(some-function arg1
    |               (do
    |                 (something)
    |                 (something-else))
    |               arg3)"

    "(some-function arg1 (arg2) (arg3))"

    "(some-function arg1 (arg2 1) (arg3 2))"

    "(some-function arg1
    |               (arg2 1 :foo)
    |               (arg3 2))"

    "(some-function (arg1 :foo)
    |               (arg2 1)
    |               (arg3 2))"

    ; string arguments
    "(some-function \"arg1\"
    |               \"arg2
    | line break\"
    |               \"arg3\")"

    ; function calls with other expressions instead of a symbol
    "((fnil foo 0) arg1 arg2 arg3)"

    "((fnil (fnil foo 0) 0) (arg1 :foo)
    |                       (arg2 1)
    |                       (arg3 2))"

    "((do
    |   (fnil foo 0)) arg1
    |                 arg2
    |                 arg3)"

    "({:foo :bar} arg1 arg2 arg3)"

    "({:foo :bar
    |  :bar :foo} arg1
    |             arg2
    |             arg3)"

    ; comments
    "(do
    |  ; a comment for the next line
    |  (some-stuff)
    |  (some-more)
    |  (still-more-stuff))"

    "(do
    |  (some-stuff)
    |  ; a comment for the next line
    |  (some-more)
    |  (still-more-stuff))"

    "(do
    |  (some-stuff)
    |  ; a comment for the next line
    |  ; this one has more than one
    |  ; line
    |  (some-more)
    |  (still-more-stuff))"

    "(some-function ; a comment
    | arg1
    | arg2)"

    "(some-function ; a comment
    | ; another comment
    | arg1
    | arg2)"

    ; comments as the last children
    "(do
    |  (something)
    | ; comment
    | ; comment2
    | )"

    "(; a comment
    | )"

    ; shouldn't turn comments into trailing comments
    "(some-function
    | ; a comment
    | ; another comment
    | arg1
    | arg2)"

    "; top level comment
    |(ns some-namespace)
    |
    |; top level comment at the end"

    "(do
    |  ; a comment for the next line
    |  (some-stuff) ; trailing comment
    |  (some-more)
    |  (still-more-stuff) ; more
    | )"

    "{:foo :bar ; trailing comment
    | ; next line comment
    | }"

    ; newlines between comments are preserved, because
    ; block commenting out in, for instance, Emacs will
    ; cause such blank lines, and they shouldn't be removed
    "(clojure.test/are foo
    |                  bar
    |  1
    |  2
    |  ; foo
    |
    |  ; bar
    |  3
    |  4)"

    ; discarded forms
    "(foobar #_arg1 arg2 arg3)"

    "(foobar #_arg1 #_arg2 #_arg3)"

    "{:key :value
    | #_key
    | :another :value}"

    ; chained discard forms
    "{#_#_#_:foo 1 2}"

    "{#_#_ #_ :foo 1 2}"

    "{#_#_#_#_:foo 1
      :bar 2}"

    ;; TODO: smart discard positioning
    ;; "{:key :value
    ;; | #_:key2 :#_2
    ;; | :another :value}"

    ;; "{:key :value
    ;; | :another #_:value
    ;; | #_:key2  :value}"

    ; TODO: smart discard positioning
    ;; "(do
    ;; |  #_#_(some-stuff)
    ;; |  (more-stuff)
    ;; |  (some-more-stuff))"

    ; metadata
    "^:bar ^:foo ^String foobar"

    "^:bar ^:foo ^String (do
    |                      (some-stuff))"

    "^{:foo :bar}
    |foobar"

    "^{:foo :bar
    |  :tag String}
    |foobar"

    "(foo-bar arg1
    |         ^String (do
    |                   (some-stuff))
    |         ^{:foo :bar}
    |         arg3)"

    ; deprecated metadata
    "#^:foo ^:bar ^String foobar"

    "#^:bar #^:foo ^String (do
    |                        (some-stuff))"

    "#^{:foo :bar}
    |^{:another :value}
    |foobar"

    "^{:foo :bar
    |  :tag String}
    |foobar"

    "(foo-bar arg1
    |         ^String (do
    |                   (some-stuff))
    |         ^{:foo :bar}
    |         arg3)"

    ; metadata duplication in deprecated metadata
    "#^:foo ^:foo foobar"

    "#^{:foo true}
    |^{:foo true}
    |foobar"

    ; deref
    "(foo arg1 @arg2 arg3)"

    "(foo arg1
    |     @(do
    |        (do-something))
    |     arg3)"

    ; namespaced_map
    "#:test{:foo :bar
    |       :foo2 :bar2}"

    ; regex
    "#\"[a-z]+\""

    "(foo arg1
    |     #\"[a-z]+\"
    |     (do
    |       (foobar))
    |     arg3)"

    ; auto resolve
    "::foobar"

    "(foo arg1
    |     ::foo
    |     (do
    |       (foobar))
    |     ::bar)"

    ; quote
    "(foo 'arg1 'arg2 'arg3)"

    "(foo arg1
    |     'foo
    |     (do
    |       (foobar))
    |     'bar)"

    "'(some list elements)"

    "(foo arg1
    |     arg2
    |     '(do
    |        (foobar))
    |     'bar)"

    ; var quote
    "(foo #'arg1 #'arg2 #'arg3)"

    "(foo arg1
    |     #'foo
    |     (do
    |       (foobar))
    |     #'bar)"

    "#'(some list elements)"

    "(foo arg1
    |     arg2
    |     #'(do
    |         (foobar))
    |     #'bar)"

    ; backtick
    "(foo `arg1 `arg2 `arg3)"

    "(foo arg1
    |     `foo
    |     (do
    |       (foobar))
    |     `bar)"

    "`(some list elements)"

    "(foo arg1
    |     arg2
    |     `(do
    |        (foobar))
    |     `bar)"

    ; unquote
    "`(foo ~arg1 ~arg2 ~arg3)"

    "`(foo arg1
    |      ~foo
    |      (do
    |        (foobar))
    |      ~bar)"

    "~(some list elements)"

    "`(foo arg1
    |      arg2
    |      ~(do
    |         (foobar))
    |      ~bar)"

    ; unquote splicing
    "`(foo ~@arg1 ~@arg2 ~@arg3)"

    "`(foo arg1
    |      ~@foo
    |      (do
    |        (foobar))
    |      ~@bar)"

    "~@(some list elements)"

    "`(foo arg1
    |      arg2
    |      ~@(do
    |          (foobar))
    |      ~@bar)"

    ; tag
    "#inst \"2018-01-01T00:00:00.000\""

    "#foobar (do
    |          (something))"

    ; symbolic
    "##NaN"

    "##Inf"

    "##-Inf"

    ; eval
    "#=foobar"

    "#=(foobar)"

    "#=(do
    |    (foobar))"

    ; reader conditionals
    "#?(:clj Double/NaN
    |   :cljs js/NaN
    |   :default nil)"

    ; reader conditionals splicing
    "(foobar arg1
    |        #?@(:clj Double/NaN
    |            :cljs js/NaN
    |            :default nil)
    |        arg3)"

    ; exempt form
    "(do
    |  #_no-monk (-> foo :first :second :third))"

    "(do
    |  #_no-monk
    |  (-> foo :first :second :third
    |      (conj [ 1 2   3])))"

    "(do
    |  #_no-monk
    |     (-> foo :first :second :third
    |           (conj [ 1 2   3])))"

    "(foobar arg1
    |        #_no-monk
    |          (-> foo :first :second :third
    |            (conj [ 1 2   3]))
    |        arg3)"

    ; exempt form - short form
    "(do
    |  #_! (-> foo :first :second :third))"

    ; exempt form - doc string
    "(defn function-name
    |  #_!
    |  \"This is a doc string.
    |       It has multiple lines.
    |
    |     a line with different offset
    |
    | should be ignored.\"
    |  [arg]
    |  (do-a-thing))"

    "#_!
    |(defn function-name
    |  \"This is a doc string.
    |       It has multiple lines.
    |
    |     a line with different offset
    |
    | should be ignored.\"
    |  [arg]
    |  (do-a-thing))"

    ; defprotocol
    "(defprotocol Protocol
    |  (method-name [this])
    |  (method-name-2 [this])
    |  (method-name-3 [this]
    |                 [this a]
    |                 [this a b]
    |                 [this a b c]))"

    ; defprotocol
    "(defprotocol Protocol
    |  \"A doc-string.\"
    |  (method-name [this]))"

    ; extend-protocol
    "(extend-protocol Protocol
    |  Protocol1
    |  (method-name-1 [this]
    |   (do-something))
    |
    |  (method-name-2 [this]
    |   (do-something-else))
    |
    |  Protocol2
    |  (method-name-3 [this]
    |   (do-something))
    |
    |  (method-name-4 [this]
    |   (do-something-else)))"

    ; deftype
    "(deftype Type [arg1 arg2]
    |  Protocol1
    |  (method-name-1 [this]
    |   (do-something))
    |
    |  (method-name-2 [this]
    |   (do-something-else))
    |
    |  Protocol2
    |  (method-name-3 [this]
    |   (do-something))
    |
    |  (method-name-4 [this]
    |   (do-something-else)))"

    ; extend-type
    "(extend-type Type
    |  Protocol1
    |  (method-name-1 [this]
    |   (do-something))
    |
    |  (method-name-2 [this]
    |   (do-something-else))
    |
    |  Protocol2
    |  (method-name-3 [this]
    |   (do-something))
    |
    |  (method-name-4 [this]
    |   (do-something-else)))"

    ; block forms with extra line breaks
    "(do
    |  (first-thing)
    |
    |  (second-thing))"

    "(clojure.test/are foo
    |                  bar
    |
    |  (first-thing)
    |
    |  (second-thing))"

    ; simple
    "(foobar)"

    "()"

    "[]"

    "{}"

    "#{}"

    ; empty string
    ""

    ;
    ))

(deftest debug-format-string-changes
  (are [input output]
       (= (sut/format-string (prepare-str input) {})
          (prepare-str output))

    ""
    ""))

(deftest format-string-changes
  (are [input output]
       (= (sut/format-string (prepare-str input) {})
          (prepare-str output))

    ; remove superfluous whitespace
    "( foo bar
    | )"
    "(foo bar)"

    ; metadata unification
    "^:private ^{:foo :bar} ^String foobar"
    "^{:foo :bar
    |  :private true
    |  :tag String}
    |foobar"

    ; first occurrence of a key wins
    "^:private ^{:private :bar} ^String ^Integer foobar"
    "^{:private true
    |  :tag String}
    |foobar"

    ; dedupe inline metadata entries
    "^String ^:private ^:private ^Integer foobar"
    "^:private ^String foobar"

    ; metadata unification with deprecated metadata
    "^:private ^{:foo :bar} ^String #^:foo #^{:new :value} foobar"
    "#^{:foo true
    |   :new :value}
    |^{:foo :bar
    |  :private true
    |  :tag String}
    |foobar"

    ; first occurrence of a key wins with deprecated metadata
    "^:private ^{:private :bar} ^String ^Integer #^:deprecated foobar"
    "#^{:deprecated true}
    |^{:private true
    |  :tag String}
    |foobar"

    ; comments between metadata is just stripped
    "^String ; a comment
    |^:private foobar"
    "^:private ^String foobar"

    ; comments inside metadata map is also stripped
    ; TODO: probably worth keeping, but it's an edge case and it's tricky
    "^{:private true
    |  ; another key
    |  :tag String}
    |foobar"
    "^{:private true
    |  :tag String}
    |foobar"

    ; exempt forms stay untouched
    "#_! ^:private ^{:foo :bar} ^String #^:foo #^{:new :value} foobar"
    "#_! ^:private ^{:foo :bar} ^String #^:foo #^{:new :value} foobar"

    ; doc string alignment
    "(defn function-name
    |  \"This is a doc string.
    |   It has multiple lines.
    |
    |       a line with different offset
    |
    |   They are aligned to the first line\"
    |  [arg]
    |  (do-a-thing))"
    "(defn function-name
    |  \"This is a doc string.
    |    It has multiple lines.
    |
    |        a line with different offset
    |
    |    They are aligned to the first line\"
    |  [arg]
    |  (do-a-thing))"

    "(defn function-name
    |  \"This is a doc string.
    |   It has multiple lines.
    |
    |       a line with different offset
    |
    |They are aligned to the first line\"
    |  [arg]
    |  (do-a-thing))"
    "(defn function-name
    |  \"This is a doc string.
    |       It has multiple lines.
    |
    |           a line with different offset
    |
    |    They are aligned to the first line\"
    |  [arg]
    |  (do-a-thing))"

    "(defn function-name
    |  \"This is a doc string.
    |        It has multiple with additional indentation\"
    |  [arg]
    |  (do-a-thing))"
    "(defn function-name
    |  \"This is a doc string.
    |    It has multiple with additional indentation\"
    |  [arg]
    |  (do-a-thing))"

    "(defn function-name
    |      \"This is a doc string that'll be moved to the left.
    |        It has multiple lines.
    |           With different indentation.\"
    |  [arg]
    |  (do-a-thing))"
    "(defn function-name
    |  \"This is a doc string that'll be moved to the left.
    |    It has multiple lines.
    |       With different indentation.\"
    |  [arg]
    |  (do-a-thing))"

    ; doc-string trimming
    "(defn function-name
    |  \"    This is a doc string.
    |    It has multiple lines.
    |    And needs to be trimmed.    \"
    |  [arg]
    |  (do-a-thing))"
    "(defn function-name
    |  \"This is a doc string.
    |    It has multiple lines.
    |    And needs to be trimmed.\"
    |  [arg]
    |  (do-a-thing))"

    ; doc-string in def
    "(def ^:private var-name
    |  \"    This is a doc string.
    |  It has multiple lines.
    |  And needs to be trimmed.    \"
    |  :value)"
    "(def ^:private var-name
    |  \"This is a doc string.
    |    It has multiple lines.
    |    And needs to be trimmed.\"
    |  :value)"

    ; doc-string in defprotocol
    "(defprotocol Protocol
    |  \"    This is a doc string.
    |  It has multiple lines.
    |  And needs to be trimmed.    \"
    |  (method-name [this]))"
    "(defprotocol Protocol
    |  \"This is a doc string.
    |    It has multiple lines.
    |    And needs to be trimmed.\"
    |  (method-name [this]))"

    ; ns-block require sorting
    "(ns foobar
    |  (:require
    |   [some.where :as where]
    |   [some.what :as what]
    |   a.b.c
    |   [else.where :as foo]))"
    "(ns foobar
    |  (:require
    |   a.b.c
    |   [else.where :as foo]
    |   [some.what :as what]
    |   [some.where :as where]))"

    "(ns foobar
    |  (:require
    |   [some.where :as where]
    |   #_[a.b.c]
    |   ; multiple
    |   ; comments
    |   ; for what
    |   [some.what :as what]
    |   ; comment for foo
    |   [else.where :as foo]
    |))"
    "(ns foobar
    |  (:require
    |   #_[a.b.c]
    |   ; comment for foo
    |   [else.where :as foo]
    |   ; multiple
    |   ; comments
    |   ; for what
    |   [some.what :as what]
    |   [some.where :as where]))"

    "(ns foobar
    |  (:require
    |   [some.where :as where]
    |   #_[a.b.c]
    |   ; multiple
    |   ; comments
    |   ; for what
    |   [\"some.what\" :as what]
    |   ; comment for foo
    |   [else.where :as foo]
    |))"
    "(ns foobar
    |  (:require
    |   ; multiple
    |   ; comments
    |   ; for what
    |   [\"some.what\" :as what]
    |   #_[a.b.c]
    |   ; comment for foo
    |   [else.where :as foo]
    |   [some.where :as where]))"

    ; exempt forms should not be sorted
    "#_! (ns foobar
    |  (:require
    |   [some.where :as where]
    |   #_[a.b.c]
    |   ; multiple
    |   ; comments
    |   ; for what
    |   [\"some.what\" :as what]
    |   ; comment for foo
    |   [else.where :as foo]
    |))"
    "#_! (ns foobar
    |  (:require
    |   [some.where :as where]
    |   #_[a.b.c]
    |   ; multiple
    |   ; comments
    |   ; for what
    |   [\"some.what\" :as what]
    |   ; comment for foo
    |   [else.where :as foo]
    |))"

    "(ns foobar
    |  #_! (:require
    |    [some.where :as where]
    |    #_[a.b.c]
    |    ; multiple
    |    ; comments
    |    ; for what
    |    [\"some.what\" :as what]
    |    ; comment for foo
    |    [else.where :as foo]
    |))"
    "(ns foobar
    |  #_! (:require
    |    [some.where :as where]
    |    #_[a.b.c]
    |    ; multiple
    |    ; comments
    |    ; for what
    |    [\"some.what\" :as what]
    |    ; comment for foo
    |    [else.where :as foo]
    |))"

    ; vector
    "[ 1 2 3 4 ]"
    "[1 2 3 4]"

    "[
    |1   2
    | 3 4 ]"
    "[1 2
    | 3 4]"

    "(some-function ; a comment
    |               ; another comment
    | arg1
    | arg2)"
    "(some-function ; a comment
    | ; another comment
    | arg1
    | arg2)"

    ;
    ))

(deftest namespace-and-symbol-mapping
  (are [input symbol-mapping]
       (= (sut/format-string (prepare-str input) {:symbol-mapping symbol-mapping})
          (prepare-str input))

    ; require namespace aliases
    "(ns foo.bar
    |  (:require
    |   [clojure.core :as cc]))
    |
    |(cc/defn function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2 arg3))"
    {}

    ; resolved to somewhere.else/defn-, so treated as a function
    "(ns foo.bar
    |  (:require
    |   [somewhere.else :refer [defn-]]))
    |
    |(defn- function-name
    |       [arg1 arg2 arg3]
    |       (some-stuff arg1 arg2 arg3))"
    {}

    ; resolved to foo.bar/defn2, which is aliased to clojure.core/defn
    "(ns foo.bar
    |  (:use
    |   [another.one]
    |   [foo.bar :only [defn2]]
    |   [some.namespace]))
    |
    |(defn2 function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2 arg3))"
    {'foo.bar/defn2 'clojure.core/defn}

    ; NOT resolved to foo.bar/defn2, because it is excluded,
    ; so the mapping is ignored
    "(ns foo.bar
    |  (:use
    |   [another.one]
    |   [foo.bar :exclude [defn2]]
    |   [some.namespace]))
    |
    |(defn2 function-name
    |       [arg1 arg2 arg3]
    |       (some-stuff arg1 arg2 arg3))"
    {'foo.bar/defn2 'clojure.core/defn}

    ; resolved to some.namespace/defn, because it is in the mapping,
    ; which makes it behave like clojure.core/do
    "(ns foo.bar
    |  (:use
    |   [another.one]
    |   [foo.bar]
    |   [some.namespace]))
    |
    |(defn
    |  function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2 arg3))"
    {'some.namespace/defn 'clojure.core/do}

    ; resolved to some.namespace/defn, as some.namespace takes
    ; precedence over another.one, so it behaves like clojure.core/do,
    ; not like clojure.core/->
    "(ns foo.bar
    |  (:use
    |   [another.one]
    |   [foo.bar]
    |   [some.namespace]))
    |
    |(defn
    |  function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2 arg3))"
    {'some.namespace/defn 'clojure.core/do
     'another.one/defn 'clojure.core/as->}

    ; require with :refer :all should behave pretty much like a :use
    "(ns foo.bar
    |  (:require
    |   [some.namespace :refer :all]))
    |
    |(defn
    |  function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2 arg3))"
    {'some.namespace/defn 'clojure.core/do}

    ;
    ))
