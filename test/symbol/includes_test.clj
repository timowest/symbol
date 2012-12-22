;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.includes-test
  (:require [symbol.types :as types])
  (:use [symbol.includes :only (include)]        
        midje.sweet))  

(defn get-types
  [env form]
  (map second (filter #(= (first %) form) env)))

(def get-type (comp first get-types))

(def math (include "math.h")) ; 228
(def cmath (include "cmath")) ; 350
(def stdio (include "stdio.h")) ; 118
(def iostream (include "iostream")) ; 1059

(doseq [[name type](concat math cmath stdio iostream)]
  (if (not type)
    (throw (IllegalStateException. (str "No type for " name)))))

; C includes

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

(facts "iostream"   
  (get-type iostream 'cin) =>  '(basic_istream char (std/char_traits char))
  (get-type iostream 'cout) => '(basic_ostream char (std/char_traits char))
  (get-type iostream 'cerr) => '(basic_ostream char (std/char_traits char))
  (get-type iostream 'istream) => '(typedef istream (basic_istream char (std/char_traits char)))
  (get-type iostream 'ostream) => '(typedef ostream (basic_ostream char (std/char_traits char)))
  (get-type iostream '(basic_istream char (std/char_traits char))) => (complement nil?))

; C++ includes

(facts "iostream methods"
  (let [istream-env (nth (get-type iostream '(basic_istream char (std/char_traits char))) 2)]
    (get-types istream-env 'readsome) => '((method ((pointer char) int) int))))
