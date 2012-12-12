(ns example5
  (include "iostream"))

(def PI 3.14159)
(def NEWLINE "\n")

(defn main []
  (let [r 5.0
        circle (* 2 PI r)]
    (<< cout circle)
    (<< cout NEWLINE)))