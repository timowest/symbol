(ns helloworld
  (include "iostream"))
  
(defmacro println
  [& args]
  `(<< cout ~@args endl))

(defn main []
  (println "Hello World")
  (println "Welcome to C++ Programming")) 