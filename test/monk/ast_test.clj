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

(def ^:private value-test-fixture
  (sut/parse "(foo arg1 arg2 {:foo :bar :bar 100})"))

(deftest value-test
  (are [ast path value]
       (= (sut/value (sut/make-pointer path ast)) value)

    value-test-fixture
    []
    [:code
     [:list
      [:symbol "foo"]
      [:whitespace " "]
      [:symbol "arg1"]
      [:whitespace " "]
      [:symbol "arg2"]
      [:whitespace " "]
      [:map
       [:keyword ":foo"]
       [:whitespace " "]
       [:keyword ":bar"]
       [:whitespace " "]
       [:keyword ":bar"]
       [:whitespace " "]
       [:number "100"]]]]

    value-test-fixture
    [1]
    [:list
     [:symbol "foo"]
     [:whitespace " "]
     [:symbol "arg1"]
     [:whitespace " "]
     [:symbol "arg2"]
     [:whitespace " "]
     [:map
      [:keyword ":foo"]
      [:whitespace " "]
      [:keyword ":bar"]
      [:whitespace " "]
      [:keyword ":bar"]
      [:whitespace " "]
      [:number "100"]]]

    value-test-fixture
    [0]
    nil

    value-test-fixture
    [2]
    nil

    value-test-fixture
    [1 1]
    [:symbol "foo"]

    value-test-fixture
    [1 1 0]
    nil

    value-test-fixture
    [1 1 1]
    nil

    value-test-fixture
    [1 2]
    [:whitespace " "]

    value-test-fixture
    [1 7 1]
    [:keyword ":foo"]

    ;
    ))

(deftest pointer-movement-test
  (are [function value]
       (= (sut/value (function (sut/make-pointer [] value-test-fixture))) value)

    identity
    [:code
     [:list
      [:symbol "foo"]
      [:whitespace " "]
      [:symbol "arg1"]
      [:whitespace " "]
      [:symbol "arg2"]
      [:whitespace " "]
      [:map
       [:keyword ":foo"]
       [:whitespace " "]
       [:keyword ":bar"]
       [:whitespace " "]
       [:keyword ":bar"]
       [:whitespace " "]
       [:number "100"]]]]

    sut/down
    [:list
     [:symbol "foo"]
     [:whitespace " "]
     [:symbol "arg1"]
     [:whitespace " "]
     [:symbol "arg2"]
     [:whitespace " "]
     [:map
      [:keyword ":foo"]
      [:whitespace " "]
      [:keyword ":bar"]
      [:whitespace " "]
      [:keyword ":bar"]
      [:whitespace " "]
      [:number "100"]]]

    #(-> % sut/down sut/left)
    nil

    #(-> % sut/down sut/right)
    nil

    #(-> % sut/down sut/down)
    [:symbol "foo"]

    #(-> % sut/down sut/down sut/down)
    nil

    #(-> % sut/down sut/down sut/down sut/left)
    nil

    #(-> % sut/down sut/down sut/right)
    [:symbol "arg1"]

    #(-> % sut/down sut/down sut/right sut/right sut/right sut/down)
    [:keyword ":foo"]

    #(-> % sut/down sut/down sut/left sut/left sut/down sut/up sut/up)
    [:list
     [:symbol "foo"]
     [:whitespace " "]
     [:symbol "arg1"]
     [:whitespace " "]
     [:symbol "arg2"]
     [:whitespace " "]
     [:map
      [:keyword ":foo"]
      [:whitespace " "]
      [:keyword ":bar"]
      [:whitespace " "]
      [:keyword ":bar"]
      [:whitespace " "]
      [:number "100"]]]

    ;
    ))
