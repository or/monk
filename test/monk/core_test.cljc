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

(defn- for-comparison
  [s]
  (str/split s #"\n"))

(deftest reformat-string
  (are [input output]
       (= (for-comparison (sut/reformat-string (prepare-str input)))
          (for-comparison (prepare-str output)))

    "(ns   foo.bar
    | (:require  [clojure.string :as  str] )
    |  )  "
    "(ns foo.bar
    |  (:require
    |   [clojure.string :as str]))"

    "{ :key
 :value :another-key (another-value arg1 arg2) }  "
    "{:key :value
    | :another-key (another-value arg1 arg2)}"

    ;;
    ))
