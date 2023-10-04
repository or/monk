(ns monk.processor)

(defn default
  [{:keys [index]
    :as context}]
  [(if (zero? index)
     {:newlines 0
      :spaces 0}
     {:newlines 0
      :spaces 1})
   context])

(defn map-form
  [{:keys [index]
    :as context}]
  [(cond
     (zero? index) {:newlines 0
                    :spaces 0}
     (even? index) {:newlines 1
                    :spaces 1}
     :else {:newlines 0
            :spaces 1})
   context])
