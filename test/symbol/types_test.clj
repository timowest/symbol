(ns symbol.types-test
  (:require [clojure.core.logic :as logic]
            [clojure.walk :as walk])
  (:use symbol.types 
        midje.sweet))

; TODO make this the base environment
(def env (to-env  '((set!  (fn [A A] void))
                    (pset! (fn [(pointer A) A] void))
                    (pset! (fn [(pointer A) long A] void))
                    (pref  (fn [(pointer A long)] A))
                    (not   (fn [boolean] boolean))
                    (<     (fn [A A] boolean))
                    (>     (fn [A A] boolean))
                    (<=    (fn [A A] boolean))
                    (>=    (fn [A A] boolean))
                    (+     (fn [A A] A))
                    (-     (fn [A A] A))
                    (*     (fn [A A] A))
                    (/     (fn [A A] A))
                    
                    (substr (fn [string long] string)))))

(def env2 (to-env '{person (object ((name string) 
                            (age long)
                            (olderThan (fn [long] boolean))))}))

(def env3 (to-env '{Person (class ((name string) 
                           (age long)
                           (:new [string])
                           (:new [string long])))}))

(facts "if"
  (typeof env '(if (<= 1 3) 2 5)) => 'long
  (typeof '(if true 1)) => 'long
  (typeof '(if true 1 2)) => 'long
  (typeof '(if true (let* [b 1] b))) => 'long
  (typeof '(if true (let* [x 15 z "s"] z))) => 'string)

(facts "fn*"
  (typeof env '(fn* ([a b] (+ a b)))) => '(fn [_.0 _.0] _.0)
  (typeof env '(fn* ([a] (substr a 1)))) => '(fn [string] string)
  (typeof env '(fn* ([a] (fn* ([b] (+ a b)))))) => '(fn [_.0] (fn [_.0] _.0)))

(facts "fn* annotated"
  (typeof env '(fn* ([^int a] a))) => '(fn [int] int))

(facts "fn* generic"
  (typeof env '(fn* ([a] a))) => '(fn [_.0] _.0)
  (typeof env '(fn* ([a b] (+ a b)))) => '(fn [_.0 _.0] _.0)
  (typeof env '(fn* ([a] (+ a 1)))) => '(fn [long] long)
  (typeof env '(fn* ([a] (+ a 1.0)))) => '(fn [double] double))
  
(facts "let*"
  (typeof '(let* [a 1 b "x"] a)) => 'long
  (typeof '(let* [a 1 b "x"] b)) => 'string)

(facts "let* annotated"
  (typeof env '(let* [^int a 1] (+ a 2))) => 'long ; FIXME
  (typeof env '(let* [^int a 1 ^int b 2] (+ a b))) => 'long) ; FIXME

(facts "loop*"
  (typeof env '(loop* fact [x 5] (if (<= x 1) 1 (* x  (recur fact (- x 1)))))) => 'long)

(facts "dot"
  (typeof env2 '(. person name)) => 'string
  (typeof env2 '(. person age)) => 'long
  (typeof env2 '(. person olderThan 10)) => 'boolean)

(facts "new"
  (let [person-object (list 'object (-> env3 first second second))]
    (typeof env3 '(new Person 1)) => nil
    (typeof env3 '(new Person)) => nil
    (typeof env3 '(new Person "a")) => person-object
    (typeof env3 '(new Person "a" 1)) => person-object))

(facts "apply"
  (typeof '((a (fn [long] long))) '(a 1)) => 'long
  (typeof '((+ (fn [long long] long))) '(+ 1 2)) => 'long)

(facts "def"
  (typeof env '(def fact (fn* ([x] (if (<= x 1) 1 (* x  (fact (- x 1)))))))) => '(fn [long] long)
  (typeof '(def a 1)) => 'long
  (typeof '(def b (fn* ([] 1)))) => '(fn [] long))

(facts "def annotated"
  (typeof '(def ^int a)) => 'int
  (typeof '(def + ^{:tag (fn [A A] A)} 'native)) => '(fn [_.0 _.0] _.0)
  (typeof '(def cos ^{:tag (fn [double] double)} 'native)) => '(fn [double] double))

(facts "do"
  (typeof '(do 1 2 true "abc")) => 'string
  (typeof '(do "abc" 1)) => 'long
  (typeof '(do true)) => 'boolean)         

(facts "constants"
  (typeof 1) => 'long
  (typeof "s") => 'string
  (typeof \s) => 'char
  (typeof 1.2) => 'double
  (typeof 1/2) => 'ratio)
    
  
      

