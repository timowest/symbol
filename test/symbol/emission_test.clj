(ns symbol.emission-test
  (:require [symbol.analysis :as analysis]
            [symbol.compiler :as compiler]
            [symbol.types :as types])
  (:use symbol.emission        
        midje.sweet))

(def core-env (types/to-env  
  '((set!  (fn [A A] void))
    (not   (fn [boolean] boolean))
    (println (fn [string] void))
    (<     (fn [A A] boolean))
    (+     (fn [A A] A)))))
  
(defn expand
  [form]
  (compiler/expand-all (:macros compiler/core-forms) form))

(defn cpp
  [form]
  (let [expanded  (expand form)
        converted (analysis/convert expanded)
        env (types/new-env core-env converted)]
    (emit env nil converted)))  

(facts "emit"
  (fact "let"
    (cpp '(let [a 1 b 2] (+ a b))) 
    => "int64_t G__13980 = 1;\nint64_t G__13981 = 2;\n(G__13521 + G__13522);\n")
  (fact "defn"
    (cpp '(defn identity [a] a)) => '(def identity (fn* ([a] a))))
  (fact "when"
    (cpp '(when a (println "hello") (println "world"))) 
    => "if (a) {\nprintln(\"hello\");\nprintln(\"world\");\n\n}")
  (fact "when-not"
    (cpp '(when-not a (println "hello") (println "world"))) 
    => "if (!a) {\nprintln(\"hello\");\nprintln(\"world\");\n\n}")
  (fact "cond"
    (cpp '(cond a 1 b 2 c 3)) 
    => "if (a) {\n1\n} else {\nif (b) {\n2\n} else {\nif (c) {\n3\n}\n}\n}")
  (fact "if-not"
    (cpp '(if-not a b c)) => "if (!a) {\nb\n} else {\nc\n}")
  ; and
  ; or
  (fact "->"
    (cpp '(-> a (b 1) c)) => "c(b(a, 1))")
  (fact "->>"
     (cpp '(->> a (b 1) c)) => "c(b(1, a))")
  ; if-let
  ; when-let
  (fact "dotimes"
    (cpp '(dotimes [i 5] (println i))) => 'x)
  (fact "fn generic"
    (cpp '(fn [x] x)) => '(fn* ([x] x)))
  (fact "fn typed"
    (cpp '(fn [a] (+ a 1))) => '(fn* ([a] (+ a 1))))
  (fact "loop"
    (cpp '(loop [x 4] x)) => '(loop* [x 4] x)))
