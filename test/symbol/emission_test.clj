(ns symbol.emission-test
  (:require [clojure.string :as string] 
            [symbol.analysis :as analysis]
            [symbol.compiler :as compiler]
            [symbol.types :as types])
  (:use symbol.emission        
        midje.sweet))

(def core-env (types/to-env  
  '((set!  (fn [A A] void))
    (not   (fn [boolean] boolean))
    (println (fn [A] void))
    (inc   (fn [long] long))
    (<     (fn [A A] boolean))
    (+     (fn [A A] A)))))
  
(defn expand
  [form]
  (compiler/expand-all (:macros compiler/core-forms) form))

(defn cpp
  [form]
  (let [expanded  (expand form)
        converted (analysis/convert expanded)
        env (types/new-env core-env converted)
        emitted (emit env nil converted)]
    (reduce 
      (fn [s [k v]] (string/replace s k v))
      emitted
      (zipmap
        (distinct (re-seq #"G__\d+" emitted))
        (list "_a" "_b" "_c" "_d" "_e")))))
   
(facts "emit"
  (fact "let"
    (cpp '(let [a 1 b 2] (+ a b))) 
    =>  "int64_t _a = 1;\nint64_t _b = 2;\n(_a + _b);\n")
  
  (fact "defn"
    (cpp '(defn identity [a] a)) 
    => "std::function<A(A)> identity = [](A _a){\nreturn _a;\n}\n")
  
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
  
  (fact "and"
    (cpp '(and (< 3 4) (< -1.0 1.0))) => "if ((3 < 4)) {\n(-1.0 < 1.0)\n}"
    (cpp '(and (< 0 1) (< 1 2) (< 2 3))) => "if ((0 < 1)) {\nif ((1 < 2)) {\n(2 < 3)\n}\n}")

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
    (cpp '(fn [x] x)) =>  "[](A _a){\nreturn _a;\n}")
  
  (fact "fn typed"
    (cpp '(fn [a] (+ a 1))) =>  "[](int64_t _a){\nreturn (_a + 1);\n}")
  
  (fact "loop"
    (cpp '(loop [x 4] x)) => "int64_t _a = 4;\n_b:\n_a;\n"))
