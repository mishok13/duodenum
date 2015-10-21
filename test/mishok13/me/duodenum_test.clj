(ns mishok13.me.duodenum-test
  (:require [midje.sweet :refer [fact tabular contains]]
            [mishok13.me.duodenum :as d]))

(fact
 "Empty parser does nothing"
 (d/parse (d/parser) []) => nil
 (d/parse (d/parser) nil) => nil
 (d/parse nil []) => nil
 (d/parse nil nil) => nil)

(fact
 "Option creation has plenty of edge cases")

(fact
 "Simple argument based parsing works correctly"
 (d/parse (d/parser (d/argument "foo")) ["42"]) => (contains {:arguments {"foo" "42"} :options nil :unparsed nil})
 (d/parse (d/parser (d/argument "foo" :count 1)) ["42"]) => (contains {:arguments {"foo" "42"} :options nil :unparsed nil})
 (d/parse (d/parser (d/argument "foo" :count 1)) ["42" "27"]) => (contains {:arguments {"foo" "42"} :unparsed ["27"]})
 (d/parse (d/parser (d/argument "foo" :count 1)) []) => (contains {:arguments {"foo" nil} :unparsed nil :errors [{:argument "foo" :kind :not-enough-arguments}]}))

(fact
 "Parsing multiple arguments works correctly"
 (d/parse (d/parser (d/argument "foo") (d/argument "bar")) []) => (contains {:arguments {"foo" nil "bar" nil} :errors [{:argument "foo", :kind :not-enough-arguments} {:argument "bar", :kind :not-enough-arguments}]})
 (d/parse (d/parser (d/argument "foo") (d/argument "bar")) ["42"]) => (contains {:arguments {"foo" "42" "bar" nil} :errors [{:argument "bar", :kind :not-enough-arguments}]})
 (d/parse (d/parser (d/argument "foo") (d/argument "bar")) ["42" "43"]) => (contains {:arguments {"foo" "42" "bar" "43"} :errors nil :unparsed nil})
 (d/parse (d/parser (d/argument "foo") (d/argument "bar")) ["42" "43" "44"]) => (contains {:arguments {"foo" "42" "bar" "43"} :errors nil :unparsed ["44"]})
 (d/parse (d/parser (d/argument "foo") (d/argument "bar" :count 2)) ["42" "43" "44"]) => (contains {:arguments {"foo" "42" "bar" ["43" "44"]} :errors nil :unparsed nil}))

;; (fact
;;  "Simple option parsing works correctly"
;;  (d/parse (d/parser (d/option "foo" :short "-f")) []) => {:arguments nil :options {"foo" false}}
;;  (d/parse (d/parser (d/option "foo" :short "-f")) ["-f"]) => {:arguments nil :options {"foo" true}}
;;  (d/parse (d/parser (d/option "foo" :short "-f" :kind :boolean)) ["-f"]) => {:arguments nil :options {"foo" true}}
;;  (d/parse (d/parser (d/option "foo" :short "-f" :kind :counter)) ["-f"]) => {:arguments nil :options {"foo" 1}}
;;  (d/parse (d/parser (d/option "foo" :short "-f")) ["-ff"]) => {:arguments nil :options {"foo" true}}
;;  (d/parse (d/parser (d/option "foo" :short "-f" :kind :accumulator)) ["-ff"]) => {:arguments nil :options {"foo" ["f"]}}
;;  (d/parse (d/parser (d/option "foo" :short "-f")) ["-fff"]) => {:arguments nil :options {"foo" true}}
;;  (d/parse (d/parser (d/option "foo" :short "-f" :kind :counter)) ["-fff"]) => {:arguments nil :options {"foo" 3}}
;;  (d/parse (d/parser (d/option "foo" :short "-f" :kind :accumulator)) ["-fff"]) => {:arguments nil :options {"foo" ["ff"]}})

;; (tabular
;;  (fact
;;   "Accumulator options work correctly"
;;   (d/parse (d/parser ?option) ?args) => (contains ?result))
;;  ?option ?args ?result
;;  (d/option "foo" :short "-f") [] nil)

;; (fact
;;  "Mixed option and argument parsing works"
;;  (d/parse (d/parser (d/option "foo" :short "-f") (d/argument "bar")) []) => {:arguments {"bar" nil} :options {"foo" false} :errors [{:argument "bar" :kind :not-enough-arguments}]})
