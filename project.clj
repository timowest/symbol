(defproject symbol "0.1.0"
  :description "symbol"
  :dependencies [[org.clojure/clojure "1.4.0"]        
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/core.logic "0.8-alpha3"]
                 [midje "1.4.0"]]
  :profiles {:dev {:plugins [[lein-midje "2.0.0-SNAPSHOT"]]}}
  :dev-dependencies [[com.stuartsierra/lazytest "1.2.3"]])

