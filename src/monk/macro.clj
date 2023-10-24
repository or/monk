(ns monk.macro)

(defmacro defformatter
  [name detector formatter]
  (assert (and (list? detector)
               (pos? (count detector))
               (vector? (first detector)))
          (str "Second argument of defformatter must be a detector, which is a list starting with an arglist vector followed by a function body."))

  (assert (and (list? formatter)
               (pos? (count formatter))
               (vector? (first formatter)))
          (str "Third argument of defformatter must be a formatter, which is list starting with an arglist vector followed by a function body."))

  (let [[detector-args & detector-body] detector
        [formatter-args & formatter-body] formatter]
    `(def ~name
       {:name ~(str name)
        :detector (fn ~'detector
                    ~detector-args
                    ~@detector-body)
        :formatter (fn ~'formatter
                     ~formatter-args
                     ~@formatter-body)})))
