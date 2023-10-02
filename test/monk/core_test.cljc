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

    ;; ns
    "(ns foo.bar
    |  (:require
    |   [clojure.string :as str]))"

    ;; map
    "{:key :value
    | :another-key (another-value arg1 arg2)}"

    "{:key :value
    | :another-key :another-value}"

    ;; do
    "(do
    |  (one-thing)
    |  (and-another-thing))"

    "(doall
    |  (one-thing))"

    ;; defn
    "(defn function-name
    |  [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2)
    |  (more-stuff arg3))"

    "(defn ^{:meta true} function-name
    |  \"Some doc string\"
    |  [arg1 arg2]
    |  (body))"

    "(defn ^:private ^:and-other function-name
    |  \"Some doc string\"
    |  [arg1 arg2]
    |  (body))"

    "(defn function-name
    |  \"Some doc string
    |
    |   with
    |   multiple lines.\"
    |  [arg1 arg2]
    |  (body))"

    #_#_"(defn
    |  ^{:multiple :meta
    |    :data :values}
    |  function-name
    |  \"Some doc string\"
    |  [arg1 arg2]
    |  (body))"

      "(defn function-name
    |  \"Some doc string\"
    |  [arg1
    |   arg2
    |   {:keys [foo bar]
    |    :as multi-line-destructuring-or-long}]
    |  (body))"

    ;;
    ))
