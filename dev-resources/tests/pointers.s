(ns pointers)

; in fn signatures
(defn first [^double* doubles]
  (pref doubles 0))

(defn second [^double** doubles]
  (pref doubles 1))

; in structs
(defstruct Synth
  (freq double*)
  (ints int**))

; in array declarations
(defn fna []
  (let [a (array double* 4)]
    (delete a)))

; in casts
(defn fnb []
  (let [a (array double* 4)
        b (cast double* a)]
    (delete a)))