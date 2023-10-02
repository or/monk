(ns sample
  (:require
    [clojure.string :as str])
  (:import
    [some.where Bar Foo])
  #?(:clj (:import
            java.util.regex.Pattern)
     :cljs (:require-macros
             [cljfmt.core :refer [read-resource]])))

(defn ^String function
  "Simple doc string"
  {:version 1
   :foo :bar}
  [a b c]
  (-> a
      (->>
        (map b))))

(defn another-function
  "Multi-line doc string.

   And another."
  ([a]
   (function a nil nil))

  ([a b]
   (function a b nil))

  ([a b c]
   (function a b c)))

(let [foo :foo
      bar :bar]
  (when (nil? foo)
    (do-something :foo-is-nil))
  (function foo
            (if (nil? bar)
              0
              1)
            bar))

(assoc {}
       :foo
         1
       :bar
         4)
(-> {}
    (assoc
     :foo
       1

     :foo
       4))

(-> {}
    (assoc
     :foo
     1

     :foo
     4))

;; relevant per line: levels of nesting and number of sequential expressions
(update-in {} [:a :b] (fnil conj []) :c)

(update-in {} [:a :b]
           (fnil conj [])
           :c)

(cond
  (nil? a)
    (let [foo :foo
          bar :bar]
      (function foo bar nil))

  (zero? b)
    :foo

  :else
    :bar)

{:some-key
   (complex-value
     :that
     :needs
     :more
     :space)

 :another-key
   87}

(let [first-var
        {:value :for
         :first :var}

      second-var
        {:value :for
         :second :var}])

(-> some-values
 function-call
 (another-function-call :with :args))

{:key :value
 :another :value}

(comment

)
