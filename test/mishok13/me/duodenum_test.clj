(ns mishok13.me.duodenum-test
  (:require [midje.sweet :refer [fact tabular]]
            [mishok13.me.duodenum :as d]))

(fact
 "Empty parser does nothing"
 (d/parse (d/parser) []) => nil
 (d/parse (d/parser) nil) => nil
 (d/parse nil []) => nil
 (d/parse nil nil) => nil)

(fact
 "Simple argument based parsing works correctly"
 (d/parse (d/parser (d/argument "foo")) ["42"]) => {:arguments {"foo" "42"} :options nil :unparsed nil}
 (d/parse (d/parser (d/argument "foo" :count 1)) ["42"]) => {:arguments {"foo" "42"} :options nil :unparsed nil}
 (d/parse (d/parser (d/argument "foo" :count 1)) ["42" "27"]) => {:arguments {"foo" "42"} :options nil :unparsed ["27"]}
 (d/parse (d/parser (d/argument "foo" :count 1)) []) => {:arguments {} :options nil :unparsed nil :errors [{:argument "foo" :kind :not-enough-arguments}]})
