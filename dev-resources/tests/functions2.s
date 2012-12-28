(ns functions2
  (include "functional"))

(defn adder [rhs]
  (fn [lhs]
    (+ lhs rhs)))

(defn fn1 []
  (let [add2 (adder 2.0)]
    (fn [lhs]
      (add2 lhs))))
      