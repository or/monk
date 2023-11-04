(ns monk.namespace-test
  (:require
   [clojure.test :refer [are deftest]]
   [monk.ast :as ast]
   [monk.namespace :as sut]
   [monk.test-util :refer [prepare-str]]))

(deftest build-alias-map
  (are [input output]
       (= (-> input
              prepare-str
              ast/parse
              ast/zipper
              sut/build-alias-map)
          output)

    "(ns foo.bar
    |  (:require
    |    [clojure.core :as cc]
    |    [clojure.set :refer :all]
    |    [clojure.string :as str :refer [includes?]]
    |    [monk.ast :as ast])
    |  (:use
    |    [another.one ]
    |    [foo.bar :only [do-this and-this]]
    |    [some.namespace :exclude [do-that and-that]]))"
    {:require {:aliases {"ast" "monk.ast"
                         "cc" "clojure.core"
                         "str" "clojure.string"}
               :refer-all ["clojure.set"]
               :refer {"includes?" 'clojure.string/includes?}}
     :use [["some.namespace" {:exclude #{"do-that" "and-that"}}]
           ["foo.bar" {:only #{"do-this" "and-this"}}]
           ["another.one" {}]]}

    ;
    ))
