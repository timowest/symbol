(ns example4
  (include "iostream" "string"))

(defn main []
  (<< std/cout "This is a string")
  (let [^int x 0] x))
