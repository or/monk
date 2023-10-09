(ns monk.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [are deftest]]
   [monk.core :as sut]))

(defn- prepare-str
  [s]
  (->> (str/split s #"\n")
       (map #(str/replace % #"^ *[\\|]" ""))
       (str/join "\n")))

(deftest format-string
  (are [input]
       (= (sut/reformat-string (prepare-str input))
          (prepare-str input))

    ; ns
    "(ns foo.bar
    |  (:require
    |   [clojure.string :as str]))"

    "(ns foo.bar
    |  (:import
    |   [some.namespace Class1 Class2]))"

    ; map
    "{:key :value
    | :another-key (another-value arg1 arg2)}"

    ; vector
    "[1 2 3 4]"

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
    |   with
    |   multiple lines.\"
    |  [arg1 arg2]
    |  (body))"

    "(defn- function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2)
    |  (more-stuff arg3))"

    "(defn- function-name
    |  \"Some doc string
    |
    |   with
    |   multiple lines.\"
    |  [arg1 arg2]
    |  (body))"

    "(defn ^:private ^String function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2)
    |  (more-stuff arg3))"

    #_"(defn function-name
    |  \"Some doc string\"
    |  [arg1
    |   arg2
    |   {:keys [foo bar]
    |    :as multi-line-destructuring-or-long}]
    |  (body))"

    ; def
    "(def var-name
    |  (some-stuff arg1 arg2))"

    "(def ^:private var-name
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

    ; ->
    ; TODO: inline form, if newlines are not needed
    #_"(-> some-map :key1 :key2 :key3 (assoc :key :value))"

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

    ;;
    ))
