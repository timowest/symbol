(ns functions4
  (include "functional"))

(defn f1 [^double arg1]
  (fn [arg2 arg3]
    (+ arg1 arg2 arg3)))

(defn f2 [^double arg1]
  (let [f (f1 arg1)]
    (fn [arg2 arg3]
      (+ arg2 arg3 (f arg2 arg3)))))  