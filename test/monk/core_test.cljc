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

    ;; map
    "{:key :value
    | :another-key (another-value arg1 arg2)}"

    ;; do
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

    ; let
    "(let [foo :bar]
    |  (some-stuff foo)
    |  (more-stuff))"

    "(let [foo :bar
    |      another :thing]
    |  (some-stuff foo)
    |  (more-stuff another))"

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

    ;;
    ))
