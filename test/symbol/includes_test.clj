;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.includes-test
  (:use symbol.includes        
        midje.sweet))  

(defn get-type
  [env form]
  (let [matches (for [[f t] env :when (= f form)] t)]
    (first matches)))

(def math (include "math.h"))
(def cmath (include "cmath"))
(def stdio (include "stdio.h"))
(def iostream (include "iostream"))

(doseq [[name type](concat math cmath stdio)]
  (if (not type)
    (throw (IllegalStateException. (str "No type for " name)))))

(facts "math"
  (get-type math 'sin) => ' (fn (double) double)
  (get-type math 'sinf) => '(fn (float) float)
  (get-type math 'frexp) => '(fn (double (pointer int)) double))

(facts "cmath"
  (get-type cmath 'sin) => ' (fn (double) double)
  (get-type cmath 'sinf) => '(fn (float) float)
  (get-type cmath 'frexp) => '(fn (double (pointer int)) double))

(facts "stdio"
  (get-type stdio 'getc) => '(fn ((pointer _IO_FILE)) int)
  (get-type stdio 'gets) => '(fn ((pointer char)) (pointer char))
  (get-type stdio 'getw) => '(fn ((pointer _IO_FILE)) int))       