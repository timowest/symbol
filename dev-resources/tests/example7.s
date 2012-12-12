(ns example7
  (include "iostream"))

(defn main []
  (let [i 0]
    (<< cout "Please enter an integer value: ")
    (>> cin i)
    (<< cout "The value entered is " i)
    (<< cout "and its double is " (* i 2) ".\n")))