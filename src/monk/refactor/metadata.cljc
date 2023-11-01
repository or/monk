(ns monk.refactor.metadata
  (:require
   [clojure.zip :as z]
   [monk.ast :as ast]))

(defn- combine-metadata-entries-into-map
  [entries]
  (let [new-entries (first (reduce
                            (fn [[result seen-keys] [entry-kind entry-data]]
                              (let [new-entries (cond
                                                  (ast/is-map? entry-data) (map vec (partition 2 (remove
                                                                                                  #(or (ast/is-whitespace? %)
                                                                                                       (ast/is-comment? %))
                                                                                                  (rest entry-data))))
                                                  (ast/is-symbol? entry-data) [[[:keyword ":tag"] entry-data]]
                                                  :else [[entry-data [:symbol "true"]]])]
                                (loop [[[new-value
                                         :as new-entry] & remaining-entries] new-entries
                                       result result
                                       seen-keys seen-keys]
                                  (let [effective-key [entry-kind new-value]]
                                    (cond
                                      (nil? new-value) [result seen-keys]

                                      (get seen-keys effective-key) (recur remaining-entries result seen-keys)

                                      :else (recur remaining-entries
                                                   (conj result [entry-kind new-entry])
                                                   (conj seen-keys effective-key)))))))

                            [[] #{}]
                            entries))
        sorted-entries (sort new-entries)
        {deprecated-entries :deprecated_metadata_entry
         non-deprecated-entries :metadata_entry} (group-by first sorted-entries)]
    (cond-> []
      (seq deprecated-entries) (conj [:deprecated_metadata_entry
                                      (into [:map] (apply concat (map second deprecated-entries)))])

      (seq non-deprecated-entries) (conj [:metadata_entry
                                          (into [:map] (apply concat (map second non-deprecated-entries)))]))))

(defn- dedupe-inline-metadata-entries
  [entries]
  (sort (first (reduce
                (fn [[result seen-keys] [entry-kind key
                                         :as entry]]
                  (let [effective-key [entry-kind
                                       (if (ast/is-symbol? key)
                                         "symbol"
                                         key)]]
                    (if (get seen-keys effective-key)
                      [result seen-keys]
                      [(conj result entry) (conj seen-keys effective-key)])))
                [[] #{}]
                entries))))

(defn refactor
  [{:keys [ast]
    :as context}]
  (let [node (z/node ast)
        entries (into []
                      (remove #(or (ast/is-whitespace? %)
                                   (ast/is-comment? %)))
                      (subvec node 1 (dec (count node))))
        map-entries? (some (comp ast/is-map? second) entries)
        new-node (if map-entries?
                   (vec (concat [(first node)]
                                (combine-metadata-entries-into-map entries)
                                [(last node)]))

                   (vec (concat [(first node)]
                                (dedupe-inline-metadata-entries entries)
                                [(last node)])))]
    (assoc context :ast (z/replace ast new-node))))

(defn refactorable?
  [{:keys [ast
           exempt?]}]
  (and (ast/is-metadata? ast)
       (not exempt?)
       (not (ast/is-exempt-form? ast))))
