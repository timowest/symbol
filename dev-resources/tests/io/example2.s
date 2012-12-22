(ns example2
  (include "iostream"))

(defn main []
  (<< std/cout "Hello World! ")
  (<< std/cout "I'm a C++ programmer")
  (let [^int x 0] x))