(ns example3
  (include "iostream"))
  
(defn main []
  (let [a 5
        b 2
        a (+ a 1)
        result (- a b)]
    (<< std/cout result))
  (let [^int x 0] x))