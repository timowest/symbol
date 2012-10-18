(ns symbol.types-test
  (:require [clojure.core.logic :as logic])
  (:use symbol.types 
        midje.sweet))

(defn typed
  ([env form]
    (typeof env form))
  ([form]
    (typed [] form))) 

(def env '{+ (fn [long long] long)
           - (fn [long long] long)
           substr (fn [string long] string)})

(def any (logic/lvar))
(def env2 {'<= (list 'fn [any any] boolean)
           '* (list 'fn [any any] any)
           '+ (list 'fn [any any] any)
           '- (list 'fn [any any] any)})

(def env3 '{person (object ((name string) 
                            (age long)
                            (olderThan (fn [long] boolean))))})

(def env4 '{Person (class ((name string) 
                           (age long)
                           (:new [string])
                           (:new [string long])))})

(defn object
  [clazz]
  (list 'object (second (env4 clazz))))

(facts "if"
  (typed '(if true 1)) => 'long
  (typed '(if true 1 2)) => 'long
  (typed '(if true (let* [b 1] b))) => 'long
  (typed '(if true (let* [x 15 z "s"] z))) => 'string)

(facts "fn"
  (typed env '(fn [a b] (+ a b))) => '(fn [long long] long)
  (typed env '(fn [a] (substr a 1))) => '(fn [string] string)
  (typed env '(fn [a] (fn [b] (+ a b)))) => '(fn [long] (fn [long] long)))

(facts "fn annotated"
  (typed env '(fn [^int a] a)) => '(fn [int] int))

(facts "fn generic"
  (typed env2 '(fn [a] a)) => '(fn [_.0] _.0)
  (typed env2 '(fn [a b] (+ a b))) => '(fn [_.0 _.0] _.0)
  (typed env2 '(fn [a] (+ a 1))) => '(fn [long] long)
  (typed env2 '(fn [a] (+ a 1.0))) => '(fn [double] double))
  
(facts "let"
  (typed '(let* [a 1 b "x"] a)) => 'long
  (typed '(let* [a 1 b "x"] b)) => 'string)

(facts "let annotated"
  (typed env2 '(let* [^int a 1] (+ a 2))) => nil ; FIXME
  (typed env2 '(let* [^int a 1 ^int b 2] (+ a b))) => 'int) 

(facts "named let"
  (typed env2 '(let* fact [x 5] (if (<= x 1) 1 (* x  (fact (- x 1)))))) => 'long)

(facts "dot"
  (typed env3 '(. person name)) => 'string
  (typed env3 '(. person age)) => 'long
  (typed env3 '(. person olderThan 10)) => 'boolean)

(facts "new"
  (typed env4 '(new Person 1)) => nil
  (typed env4 '(new Person)) => nil
  (typed env4 '(new Person "a")) => (object 'Person)
  (typed env4 '(new Person "a" 1)) => (object 'Person))

(facts "apply"
  (typed '{a (fn [long] long)} '(a 1)) => 'long
  (typed '{+ (fn [long long] long)} '(+ 1 2)) => 'long)

(facts "def"
  (typed env2 '(def fact (fn [x] (if (<= x 1) 1 (* x  (fact (- x 1))))))) => '(fn [long] long)
  (typed '(def a 1)) => 'long
  (typed '(def b (fn [] 1))) => '(fn [] long))

(facts "def annotated"
  (typed '(def ^int a)) => 'int
  (typed '(def + ^{:tag (fn [A A] A)} 'native)) => '(fn [_.0 _.0] _.0)
  (typed '(def cos ^{:tag (fn [double] double)} 'native)) => '(fn [double] double))

(facts "constants"
   (typed 1) => 'long
   (typed "s") => 'string
   (typed \s) => 'char
   (typed 1.2) => 'double
   (typed 1/2) => 'ratio)
    
  
      

