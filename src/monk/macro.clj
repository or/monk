(ns monk.macro)

(defmacro defprocessor
  [name detector processor]
  (assert (and (list? detector)
               (pos? (count detector))
               (vector? (first detector)))
          (str "Second argument of defprocessor must be a detector, which is a list starting with an arglist vector followed by a function body."))

  (assert (and (list? processor)
               (pos? (count processor))
               (vector? (first processor)))
          (str "Third argument of defprocessor must be a processor, which is list starting with an arglist vector followed by a function body."))

  (let [[detector-args & detector-body] detector
        [processor-args & processor-body] processor]
    `(def ~name
       {:detector (fn ~'detector
                    ~detector-args
                    ~@detector-body)
        :processor (fn ~'processor
                     ~processor-args
                     ~@processor-body)})))
