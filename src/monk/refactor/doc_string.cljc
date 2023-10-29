(ns monk.refactor.doc-string
  (:require
   [clojure.string :as str]
   [clojure.zip :as z]
   [monk.ast :as ast]))

(defn- count-leading-spaces
  [s]
  (->> s
       (take-while #{\space})
       count))

(defn refactor
  [ast column]
  (let [[_ current-string] (z/node ast)
        previous-node (some-> ast z/left z/node)
        ; if the node is preceded by a whitespace node with newlines, which should
        ; be the case in a vast majority of situations, then we know the
        ; previous column of this element, which allows more accurate
        ; calculations, if not, then we must guess
        previous-column (when (and (ast/is-whitespace? ast)
                                   (str/includes? (second previous-node) "\n"))
                          (-> previous-node
                              second
                              (str/split #"\n")
                              last
                              count))
        lines (-> current-string
                  (subs 1 (dec (count current-string)))
                  str/trim
                  (str/split #"\n"))
        offsets-and-lengths (->> lines
                                 (drop 1)
                                 (map str/trimr)
                                 (map (juxt count-leading-spaces count))
                                 (map (fn [[offset length]]
                                        [offset (- length offset)])))
        trimmed-lines (map str/trim lines)
        column-shift (if previous-column
                       (- column previous-column)
                       0)
        base-column (+ column 2)
        shifted-offsets (map (fn [[offset length
                                   :as tuple]]
                               (if (pos? length)
                                 [(+ offset column-shift) length]
                                 tuple))
                             offsets-and-lengths)
        offsets-without-base-column (map (fn [[offset length
                                               :as tuple]]
                                           (if (pos? length)
                                             [(- offset base-column) length]
                                             tuple))
                                         shifted-offsets)
        main-shift (if (seq offsets-without-base-column)
                     (apply min (map first offsets-without-base-column))
                     0)
        offsets-shifted-and-lengths (->> offsets-without-base-column
                                         (map (fn [[offset length
                                                    :as tuple]]
                                                (if (pos? length)
                                                  [(- offset main-shift) length]
                                                  tuple))))
        adjusted-offsets (map (fn [[offset length]]
                                (if (pos? length)
                                  (+ base-column offset)
                                  0))
                              offsets-shifted-and-lengths)
        new-string (str (->> (interleave trimmed-lines
                                         (map (fn [offset]
                                                (apply str "\n" (repeat offset " "))) adjusted-offsets))
                             (apply str))
                        (last trimmed-lines))]
    (z/replace ast [:string (str "\"" new-string "\"")])))
