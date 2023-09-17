(ns monk.core-test
  (:require
   [clojure.test :refer [are is deftest]]
   [monk.core :as sut]))

(deftest reformat
  (are [input output]
       (= (sut/reformat input) output)

    "" ""))
