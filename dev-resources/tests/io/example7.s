(ns example7
  (include "iostream"))

(defn main []
  (let [i 0]
    (<< std/cout "Please enter an integer value: ")
    (>> std/cin i)
    (<< std/cout "The value entered is " i)
    (<< std/cout "and its double is " (* i 2) ".\n"))
  (int 0))