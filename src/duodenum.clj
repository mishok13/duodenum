(ns duodenum)

(defn parser
  [& opts]
  {:arguments (filter #(= :argument (:type %)) opts)})

(defn parse
  [parser args]
  (when (and parser (map? parser) (every? string? args))
    (let [[args arguments] (loop [arg-parsers (:arguments parser)
                                  args args
                                  results {}]
                             (if (seq args)
                               (if-let [current-parser (first arg-parsers)]
                                 (let [collect (:collect current-parser)]
                                   (cond
                                     (number? collect)
                                     (recur (rest arg-parsers) (drop collect args) (assoc results (:key current-parser) (take collect args)))
                                     (= :* collect)
                                     (recur (rest arg-parsers) [] (assoc results (:key current-parser) args))))
                                 [args results])
                               [args (merge results (zipmap (map :key arg-parsers) (repeat [])))]))
          options []
          unparsed (vec args)]
      {:arguments arguments :options options :unparsed unparsed})))
