(defproject symbol "0.1.0"
  :description "symbol"
  :dependencies [[org.clojure/clojure "1.5.0-RC2"]        
                 [org.clojure/core.logic "0.8.0-rc3-SNAPSHOT"]
                 [org.clojure/data.zip "0.1.1"]]
  :main symbol.main
  :java-source-paths ["srcj"]
  :profiles {:dev {:dependencies [[midje "1.4.0"]] 
                   :plugins [[lein-midje "2.0.0-SNAPSHOT"]]}
             :user {:plugins [[lein-kibit "0.0.7"]]}}
  :jvm-opts ["-Xss60m"] 
  :dev-dependencies [[com.stuartsierra/lazytest "1.2.3"]])

