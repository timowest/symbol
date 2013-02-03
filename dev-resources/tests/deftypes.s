(ns deftypes)

(comment (deftype Generic [fn ln] 
  (first [_] fn) 
  (last [_] ln)))

(deftype Concrete [n1 n2] 
  (f1 [_] (+ n1 1)) 
  (f2 [_] (+ n2 1.0)))

(defn new-concrete [n1 n2]
  (Concrete. n1 n2))