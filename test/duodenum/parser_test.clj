(ns duodenum.parser-test
  (:require
   [duodenum.parser :as p]
   [midje.sweet :refer [fact tabular] :as midje]))

(tabular
 (fact
  "Token kind is figured out correctly"
  (p/token-kind ?state ?token) => ?result)
 ?state ?token ?result
 {} "" :empty-context
 {:context {:processing :done}} "foo" :rest
 {:context {:processing :options}} "-a" :unknown-short-option
 {:context {:processing :options} :options [{:short "-a" :name "foo"}]} "-a" :short-option)
