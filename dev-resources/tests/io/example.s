(ns example
  (include "iostream"))
  
(defn main []
  (<< std/cout "Hello World!")
  (let [^int x 0] x))
