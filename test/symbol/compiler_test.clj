;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.compiler-test
  (:require [symbol.types :as types])
  (:use symbol.compiler        
        midje.sweet))

(facts "normalize"
  (normalize 'clojure.core/let*) => 'let*)

(facts "expand forms"
  (fact "normal"
    (:forms (expand-forms 'test {} '((println 1) (println 2) (println 3))))
    => '[(println 1) (println 2) (println 3)])
  
  (fact "with do"
    (:forms (expand-forms 'test {} '((do (println 1) (println 2) (println 3)))))
    => '[(println 1) (println 2) (println 3)]))
  
(defn expand
  [form]
  (expand-all (:macros core-forms) form))

(facts "expand"
       
  (facts "ns"
    (expand '(ns symbol.test (use symbol.string))) => '(do (ns* symbol.test) (use symbol.string)))
       
  (fact "let"
    (expand '(let [a 1 b 2] (+ a b))) => '(let* [a 1 b 2] (+ a b)))
  
  (fact "defn"
    (expand '(defn identity [a] a)) => '(def identity (fn* ([a] a))))
  
  (fact "when"
    (expand '(when a (println "hello") (println "world"))) 
          => '(if a (do (println "hello") (println "world"))))
  
  (fact "when-not"
    (expand '(when-not a (println "hello") (println "world"))) 
          => '(if (not a) (do (println "hello") (println "world"))))
  
  (fact "cond"
    (expand '(cond a 1 b 2 c 3)) =>  '(if a 1 (if b 2 (if c 3))))
  
  (fact "if-not"
    (expand '(if-not a b c)) => '(if (not a) b c))
  ; and
  ; or
  
  (fact "."
    (expand '(.member obj)) => '(. obj member)
    (expand '(.member obj arg)) => '(. obj member arg))
  
  (fact "new"
    (expand '(Class1. arg1 arg2)) => '(new Class1 arg1 arg2)) 
  
  (fact "->"
    (expand '(-> a (b 1) c)) => '(c (b a 1)))
  
  (fact "->>"
     (expand '(->> a (b 1) c)) => '(c (b 1 a)))
  ; if-let
  ; when-let
  
  (fact "dotimes"
    (expand '(dotimes [i 5] (println i))) => anything)
  
  (fact "fn"
    (expand '(fn [x] x)) => '(fn* ([x] x)))
  
  (fact "loop"
    (expand '(loop [x 4] x)) => '(loop* [x 4] x))
  
  (fact "struct"
    (expand '(defstruct parent (int c) (int p))) => '(def parent (struct parent (int c) (int p)))))
       

(def env (types/to-env  '((set!  (fn [A A] void))
                          (<     (fn [A A] boolean))
                          (+     (fn [A A] A))                
                          (*     (fn [A A] A))
                          (not   (fn [boolean] boolean))
                          (substr (fn [string long] string)))))

(defn typeof
  [form]
  (types/typeof env (expand form)))

(facts "types"
   (typeof '(let [a 1 b 2] (+ a b))) => 'long
   (typeof '(defn identity [a] a)) => '(fn [_.0] _.0)
   (typeof '(when true 1 2 (+ 1.0 2.1))) => 'double
   (typeof '(when-not (< 1 2) 2.0)) => 'double
   (typeof '(let [a 5] (cond (< a 1) 2 (< a 2) 3))) => 'long
   (typeof '(let [a 5] (cond (< a 1) 2 (< a 2) 3 :else 5))) => 'long
   (typeof '(if-not (< 1 2) 3.0 4.0)) => 'double
   (typeof '(-> 2 (+ 3) (* 5))) => 'long
   (typeof '(->> 2 (+ 3) (* 5))) => 'long
   ; dotimes
   (typeof '(fn [a] a)) => '(fn [_.0] _.0))

(comment (facts "read emit"
       
  (fact "simple"
    (read-emit "dev-resources/tests/simple.s")
    => ["//ns simple\n" 
        "int64_t inc(int64_t G__5465) {\nreturn (G__5465 + 1);\n}"])))
       


   
   

