(ns example5
  (include "iostream"))

(def PI 3.14159)
(def NEWLINE "\n")

(defn main []
  (let [r 5.0
        circle (* 2.0 PI r)]
    (<< std/cout circle)
    (<< std/cout NEWLINE))
  (int 0))