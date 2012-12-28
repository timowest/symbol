(ns functions3
  (include "functional"))

(def SAMPLERATE 44.100)

(defn rect-c [phase] 
  (fn [amp freq duty]
    (let [inc (/ freq SAMPLERATE)]
      (set! phase (+ phase inc))
      (when (> phase 1.0) (set! phase (- phase 1.0)))
      (if (< phase duty) amp (* -1.0 amp)))))