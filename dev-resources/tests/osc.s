(ns osc
  (include "math.h" "functional"))

(defn osc_c [phase]
  (fn [amp freq]
    (let [inc (* 3.141592 (* 2.0 (/ freq 44100.0)))]
      (set! phase (+ phase inc))
      (* amp (sin phase)))))