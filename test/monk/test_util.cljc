(ns monk.test-util
  (:require
   [clojure.string :as str]))

(defn prepare-str
  [s]
  (->> (str/split s #"\n")
       (map #(str/replace % #"^ *[\\|]" ""))
       (str/join "\n")))
