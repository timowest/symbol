(ns example6
  (include "iostream"))

(defn main []
  (let [a 2
        b 7
        c (if (> a b) a b)]
    (<< std/cout c))
  (let [^int x 0] x))