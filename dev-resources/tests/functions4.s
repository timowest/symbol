(ns functions4
  (include "math.h" "functional"))

(defn f1 [arg1]
  (fn [arg2 arg3]
    (* arg1 arg2 (sin arg3))))

(defn f2 [arg1]
  (let [f (f1 arg1)]
    (fn [arg2 arg3]
      (tanh (f arg2 arg3)))))  