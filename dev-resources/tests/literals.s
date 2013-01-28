(ns literals)

(defn double-rv []
  1.0)

(defn float-rv []
  2.0f)

(defn long-rv []
  3)

(defn int-rv []
  4i)

(defn double-arg [^double d])

(defn float-arg [^float f])

(defn long-arg [^long l])

(defn int-arg [^int i])

(defn tests []
  (double-arg (double-rv))
  (double-arg 0.0)
  (float-arg (float-rv))
  (float-arg 0.0f)
  (long-arg (long-rv))
  (long-arg 0)
  (int-arg (int-rv))
  (int-arg 0i))
  
  