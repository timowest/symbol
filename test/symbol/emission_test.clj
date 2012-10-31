;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.emission-test
  (:require [clojure.string :as string] 
            [symbol.analysis :as analysis]
            [symbol.compiler :as compiler]
            [symbol.types :as types])
  (:use symbol.emission        
        midje.sweet))

(def core-env (types/to-env  
  '((nil   void)
    (set!  (fn [A A] void))
    (pset! (fn [(pointer A) A] void))
    (pset! (fn [(pointer A) long A] void))
    (pref  (fn [(pointer A long)] A))
    (not   (fn [boolean] boolean))
    (=     (fn [A A] boolean))
    (<     (fn [A A] boolean))
    (>     (fn [A A] boolean))
    (<=    (fn [A A] boolean))
    (>=    (fn [A A] boolean))
    (+     (fn [A A] A))
    (-     (fn [A A] A))
    (*     (fn [A A] A))
    (/     (fn [A A] A))
    
    (println (fn [A] void))
    (inc   (fn [long] long)
    (dec   (fn [long] long))))))
  
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
    =>  "int64_t _a = 1;\nint64_t _b = 2;\n(_a + _b);")
  
  (fact "defn"
    (cpp '(defn identity [a] a)) 
    => "std::function<A(A)> identity = [](A _a){\nreturn _a;\n}")
  
  (fact "when"
    (cpp '(when a (println "hello") (println "world"))) 
    => "if (a) {\nprintln(\"hello\");\nprintln(\"world\");\n}")
  
  (fact "when-not"
    (cpp '(when-not a (println "hello") (println "world"))) 
    => "if (!a) {\nprintln(\"hello\");\nprintln(\"world\");\n}")
  
  (fact "cond"
    (cpp '(cond a 1 b 2 c 3)) 
    => "if (a) {\n1\n} else if (b) {\n2\n} else if (c) {\n3\n}")
  
  (fact "if-not"
    (cpp '(if-not a b c)) => "if (!a) {\nb\n} else {\nc\n}")
  
  (fact "and"
    (cpp '(and (< 3 4) (< -1.0 1.0))) => "if ((3 < 4)) {\n(-1.0 < 1.0)\n}"
    (cpp '(and (< 0 1) (< 1 2) (< 2 3))) => "if ((0 < 1)) {\nif ((1 < 2)) {\n(2 < 3)\n}\n}")
  
  (fact "or"
    (cpp '(or (< 3 4) (< -1.0 1.0))) => "if ((3 < 4)) {\ntrue\n} else {\n(-1.0 < 1.0)\n}"
    (cpp '(or (< 0 1) (< 1 2) (< 2 3))) 
    => "if ((0 < 1)) {\ntrue\n} else if ((1 < 2)) {\ntrue\n} else {\n(2 < 3)\n}")

  (fact "if and"
    (cpp '(if (and (< 3 4) (< 0 1)) (println 5)))
    => "boolean _a;\nif ((3 < 4)) {\n_a = (0 < 1);\n}\nif (_a) {\nprintln(5)\n}")
  
  (fact "if or"
    (cpp '(if (or (< 3 4) (< 0 1)) (println 5)))
    => "boolean _a;\nif ((3 < 4)) {\ntrue\n} else {\n_a = (0 < 1);\n}\nif (_a) {\nprintln(5)\n}")
  
  (fact "->"
    (cpp '(-> a (b 1) c)) => "c(b(a, 1))")
  
  (fact "->>"
     (cpp '(->> a (b 1) c)) => "c(b(1, a))")
  
  ;(fact "if-let"
  ;   (cpp '(if-let [a (< 1 2)] (println a))) => 'x)
  
  (fact "when-let"
     (cpp '(when-let [a (< 1 2)] (println a))) 
     => "boolean _a = (1 < 2);\nif (_a) {\nboolean _b = _a;\nprintln(_b);\n}")
  
  (fact "dotimes"
    (cpp '(dotimes [i 5] (println i))) 
    => "int64_t _a = 5;\nint64_t _b = 0;\n_c:\nif ((_b < _a)) {\nprintln(_b);\ngoto _c;\n}")
  
  (fact "fn generic"
    (cpp '(fn [x] x)) =>  "[](A _a){\nreturn _a;\n}")
  
  (fact "fn typed"
    (cpp '(fn [a] (+ a 1))) =>  "[](int64_t _a){\nreturn (_a + 1);\n}")
  
  (fact "loop"
    (cpp '(loop [x 4] x)) => "int64_t _a = 4;\n_b:\n_a;"))
