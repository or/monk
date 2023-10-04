(ns monk.processor
  (:require
   [monk.macro :refer [defprocessor]]
   [rewrite-clj.zip :as z]))

(defprocessor
 default

 ([_zloc]
  true)

 ([{:keys [index]
    :as context}]
  [(if (zero? index)
     {:newlines 0
      :spaces 0}
     {:newlines 0
      :spaces 1})
   context]))

(defprocessor
 map-form

 ([zloc]
  (-> zloc z/tag (= :map)))

 ([{:keys [index]
    :as context}]
  [(cond
     (zero? index) {:newlines 0
                    :spaces 0}
     (even? index) {:newlines 1
                    :spaces 1}
     :else {:newlines 0
            :spaces 1})
   context]))
