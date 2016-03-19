(ns duodenum)

(defn parser
  [& opts]
  {})

(defn parse
  [parser args]
  (when (and parser (map? parser) (every? string? args))
    {:arguments []
     :options []
     :unparsed (vec args)}))
