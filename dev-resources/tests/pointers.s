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
        b (cast double* a)
        c (pref a)
        d (pref a 0)]        
    (delete a)))

; destruct
(defn fnc []
  (let [a (array double 4)
        [b c d e] a]
    (+ b c d e)))

; wrapped special forms
(defn pref_ [a]
  (pref a))

(defn pref_ [a b]
  (pref a b))

(defn pset!_ [a b c]
  (pset! a b c))

(defn pset!_ [a b]
  (pset! a b))

(defn nil-check []
  (nil? (array int 1)))