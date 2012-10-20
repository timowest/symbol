(ns symbol.core)

; TODO convert uppercase letters to lvars
(def ^:private env 
  '((set!  (fn [A A] void))
    (pset! (fn [(pointer A) A] void))
    (pset! (fn [(pointer A) long A] void))
    (pref  (fn [(pointer A long)] A))
    (<     (fn [A A] boolean))
    (>     (fn [A A] boolean))
    (<=    (fn [A A] boolean))
    (>=    (fn [A A] boolean))
    (+     (fn [A A] A))
    (-     (fn [A A] A))
    (*     (fn [A A] A))
    (/     (fn [A A] A))))