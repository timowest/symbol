(ns functions5
  (include "functional"))

(defn plus [x]
  (fn [y]
    (+ x y)))

(defn plusx [x]
  (let [xx x]
    (fn [y]
      (let [yy y]
        (+ xx yy)))))

(defn plus1 []
  (let [x 1]
    (fn [y]
      (+ x y))))

(defn plus1x [x]
  (let [xx (+ x 1)]
    (fn [y]
      (+ xx y))))

(defn minus [x]
  (fn [y]
    (- x y)))

(defn dspa [max-delay]
  (let [line (array double max-delay)
	      time 0
	      delay max-delay]
    (fn [x]
      (let [n (% time delay)
            delayed (pref line n)
            y 0.0]
        (pset! line 0 0.0)
        (set! time (+ time 1))
        y))))