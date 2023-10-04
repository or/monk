(ns monk.processor
  (:require
   [rewrite-clj.zip :as z]))

(def default
  {:detector (fn detector
               [_zloc]
               true)
   :processor (fn processor
                [{:keys [index]
                  :as context}]
                [(if (zero? index)
                   {:newlines 0
                    :spaces 0}
                   {:newlines 0
                    :spaces 1})
                 context])})

(def map-form
  {:detector (fn detector
               [zloc]
               (-> zloc z/tag (= :map)))
   :processor (fn processor
                [{:keys [index]
                  :as context}]
                [(cond
                   (zero? index) {:newlines 0
                                  :spaces 0}
                   (even? index) {:newlines 1
                                  :spaces 1}
                   :else {:newlines 0
                          :spaces 1})
                 context])})
