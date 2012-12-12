(defproject symbol "0.1.0"
  :description "symbol"
  :dependencies [[org.clojure/clojure "1.4.0"]        
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/core.logic "0.8-alpha3"]
                 [org.clojure/data.zip "0.1.1"]
                 [midje "1.4.0"]]
  :profiles {:dev {:plugins [[lein-midje "2.0.0-SNAPSHOT"]]}
             :user {:plugins [[lein-kibit "0.0.7"]]}}
  :dev-dependencies [[com.stuartsierra/lazytest "1.2.3"]])

