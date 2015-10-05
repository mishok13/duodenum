(defproject mishok13.me/duodenum "0.1.0-SNAPSHOT"
  :description "Duodendum: A parser for command line arguments"
  :url "http://github.com/mishok13/duodemdum"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :profiles {:dev {:dependencies [[midje "1.7.0"]]
                   :plugins [[lein-midje "3.1.3"]]}})
