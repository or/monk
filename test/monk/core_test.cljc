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

(deftest reformat-string
  (are [input output]
       (= (sut/reformat-string (prepare-str input))
          (prepare-str output))

    "(ns   foo.bar
    | (:require  [clojure.string :as  str] )
    |  )  "
    "(ns foo.bar
    |  (:require
    |   [clojure.string :as str]))"

    "{ :key
    |      :value :another-key (another-value arg1 arg2) }  "
    "{:key :value
    | :another-key (another-value arg1 arg2)}"

    "{ :key  :value :another-key :another-value }  "
    "{:key :value
    | :another-key :another-value}"

    "(do (one-thing) (and-another-thing))"
    "(do
    |  (one-thing)
    |  (and-another-thing))"

    "(doall (one-thing))"
    "(doall
    |  (one-thing))"

    "(defn function-name [arg1 arg2 arg3] (some-stuff arg1 arg2) (more-stuff arg3))"
    "(defn function-name
    |      [arg1 arg2 arg3]
    |  (some-stuff arg1 arg2)
    |  (more-stuff arg3)))"

    ;;
    ))

#_(deftest str-diff
    (are [expected output]
         (= (prepare-str output) (prepare-str expected))

      "foooink yo"
      "foobar yo"

      "foo yo
    |bar"
      "foo
    |yo bar"

      "(defn escape-html-document
    |  \"Escapes special characters into fipp :span/:escaped nodes\"
    |  [document]
    |  (postwalk escape-html-node document))"
      "(defn escape-htmlx-document
    |  \"Escapes characters into fipp :span/:escaped nodes\"
    |  [document]
    |  a new line
    |  (postwalk escape-html-node document))"

      "</span>"
      "foobar"
    ;;
      ))
