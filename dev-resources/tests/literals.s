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
  (float-arg (float-rv))
  (long-arg (long-rv))
  (int-arg (int-rv)))
  
  