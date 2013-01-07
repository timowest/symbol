;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.types-test
  (:require [clojure.core.logic :as logic]
            [clojure.walk :as walk]
            [symbol.compiler :as compiler])
  (:use symbol.types 
        midje.sweet))

(def env 
  (merge
    compiler/core-env
    '{dec   [(fn [long] long)]
      inc   [(fn [long] long)]
      println [(fn [A] void)]             
      substr [(fn [string long] string)]}))

(def env2 
  (merge 
    compiler/core-env
    '{person [(pointer Person)]                    
      Person [(class 
                Person 
                {name [string] 
                 age  [long]
                 olderThan [(method [long] boolean)]
                 :new [[string] [string long]]})]}))

(facts "nil"
  (typeof env nil) => 'void)

(facts "if"
  (typeof env '(if (<= 1 3) 2 5)) => 'long
  (typeof '(if true 1)) => 'long
  (typeof '(if true 1 2)) => 'long
  (typeof '(if true (let* [b 1] b))) => 'long
  (typeof '(if true (let* [x 15 z "s"] z))) => 'string)

(facts "inline if"
  (typeof env '((if (< 0 1) inc dec) 5)) => 'long
  (typeof env '(let* [a (if (< 0 1) inc dec)] (a 5))) => 'long)

;(facts "and"
;  (typeof '(let* [x (< 3 4)] (if x (< -1.0 1.0) x))) => nil) ; FIXME

(facts "fn*"
  (typeof env '(fn* ([a b] (+ a b)))) => '(fn [_0 _0] _0)
  (typeof env '(fn* ([a] (substr a 1)))) => '(fn [string] string)
  (typeof env '(fn* ([a] (fn* ([b] (+ a b)))))) => '(fn [_0] (fn [_0] _0)))

(facts "fn* annotated"
  (typeof env '(fn* ([^int a] a))) => '(fn [int] int))

(facts "fn* generic"
  (typeof env '(fn* ([a] a))) => '(fn [_0] _0)
  (typeof env '(fn* ([a b] (+ a b)))) => '(fn [_0 _0] _0)
  (typeof env '(fn* ([a] (+ a 1)))) => '(fn [long] long)
  (typeof env '(fn* ([a] (+ a 1.0)))) => '(fn [double] double))

(facts "complex fn"
  (typeof env '(fn* ([phase] 
                 (fn* ([amp freq] 
                   (let* [i (* 3.141592 (* 2.0 (/ freq 44100.0)))] 
                     (set! phase (+ phase i)) 
                     (* amp phase)))))))
  => '(fn (double) (fn (double double) double)))
  
(facts "inline fn"
  (typeof env '((fn* ([a] (+ a 1))) 1)) => 'long)

(facts "let*"
  (typeof '(let* [^int x 0] x)) => 'int
  (typeof '(let* [a 1 b "x"] a)) => 'long
  (typeof '(let* [a 1 b "x"] b)) => 'string)

(facts "let* annotated"
  ;(typeof env '(let* [^int a 1] (+ a 2))) => nil ; FIXME
  (typeof env '(let* [^int a 1 ^int b 2] (+ a b))) => 'int)

(facts "factorial"
  (typeof env '(fn* fact([x]
                  (if (<= x 1) 1 (* x (fact (- x 1)))))))
  => '(fn [long] long))

(facts "factorial optimized"
  (typeof env '(fn* ([x]
                  (loop* xx [n x f 1]
                    (if (= n 1) f (recur* xx (dec n) (* f n)))))))
  => '(fn [long] long))

(facts "dotimes"
  (typeof env '(loop* x [b 0]
                 (if (< b 5) (do (println b) (recur* x (inc b))))))
  => 'void)
  
(facts "dot"
  (typeof env2 '(. person name)) => 'string
  (typeof env2 '(. person age)) => 'long
  (typeof env2 '(. person olderThan 10)) => 'boolean)

(facts "new"
  ;(typeof env2 '(new Person 1)) => nil
  ;(typeof env2 '(new Person)) => nil
  (typeof env2 '(new Person "a")) => '(pointer Person)
  (typeof env2 '(new Person "a" 1)) => '(pointer Person))

(facts "apply"
  (typeof '{a [(fn [long] long)]}'(a 1)) => 'long
  (typeof '{+ [(fn [long long] long)]} '(+ 1 2)) => 'long)

(facts "def"
  (typeof env '(def fact (fn* ([x] (if (<= x 1) 1 (* x  (fact (- x 1)))))))) => '(fn [long] long)
  (typeof env '(def adder (fn* ([rhs] (fn* ([lhs] (+ lhs rhs))))))) => '(fn [_0] (fn [_0] _0))
  (typeof '(def a 1)) => 'long
  (typeof '(def b (fn* ([] 1)))) => '(fn [] long))

(facts "def annotated"
  (typeof '(def ^int a)) => 'int
  (typeof '(def + ^{:tag (fn [A A] A)} 'native)) => '(fn [_0 _0] _0)
  (typeof '(def cos ^{:tag (fn [double] double)} 'native)) => '(fn [double] double))

(facts "do"
  (typeof '(do 1 2 true "abc")) => 'string
  (typeof '(do "abc" 1)) => 'long
  (typeof '(do true)) => 'boolean)         

(facts "arrays"
  (typeof '(array long 5)) => '(pointer long))

(facts "struct"
  (typeof '(struct parent (int i1) (int2))) => '(struct parent ((int i1) (int2))))

(facts "constants"
  (typeof 1) => 'long
  (typeof "s") => 'string
  (typeof \s) => 'char
  (typeof 1.2) => 'double
  (typeof 1/2) => 'ratio)
   
(facts "math include"
  (typeof '(do (include "math.h") (sin 2.0))) => 'double)

