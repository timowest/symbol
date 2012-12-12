(ns example6
  (include "iostream"))

(defn main []
  (let [a 2
        b 7
        c (if (> a b) a b)]
    (<< cout c)))