(ns monk.ast-test
  (:require
   [clojure.test :refer [are deftest]]
   [monk.ast :as sut]))

(deftest parse-test
  (are [input output]
       (= (sut/parse input) output)

    "(foo arg1 arg2 arg3)"
    [:code
     [:list
      [:symbol "foo"]
      [:whitespace " "]
      [:symbol "arg1"]
      [:whitespace " "]
      [:symbol "arg2"]
      [:whitespace " "]
      [:symbol "arg3"]]]

    "{:foo :bar\n:bar 100}"
    [:code
     [:map
      [:keyword ":foo"]
      [:whitespace " "]
      [:keyword ":bar"]
      [:whitespace "\n"]
      [:keyword ":bar"]
      [:whitespace " "]
      [:number "100"]]]

    ;
    ))
