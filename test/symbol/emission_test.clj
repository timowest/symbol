;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.emission-test
  (:refer-clojure :exclude [munge])
  (:require [clojure.string :as string] 
            [symbol.analysis :as analysis]
            [symbol.compiler :as compiler]
            [symbol.common :as common]
            [symbol.types :as types])
  (:use symbol.emission        
        midje.sweet))

(def core-env 
  (merge 
    common/core-env
    '{println [(fn [_0] void)]
      a       [long]
      b       [(fn [long long] long)] 
      c       [(fn [long] long)]
      d       [boolean]
      inc     [(fn [long] long)]
      dec     [(fn [long] long)]}))
  
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

(defn cpp-pprint
  [form]
  (print (format-cpp (cpp form))))
   
(facts "emit"
  (fact "let"
    (cpp '(let [a 1 b 2] (+ a b))) 
    =>  "long _a = 1;\nlong _b = 2;\n(_a + _b);")  
  (fact "def"
    (cpp '(def e 123.0)) => "const double e = 123.0;"
    (cpp '(def inc (fn* ([x] (+ x 1))))) => "long inc(long _a) {\nreturn (_a + 1);\n}\n")  
  (fact "defn (non-generic)"
    (cpp '(defn inc2 [a] (+ a 1))) 
    => "long inc2(long _a) {\nreturn (_a + 1);\n}\n")  
  (fact "defn (generic)"
    (cpp '(defn identity [a] a)) 
    => "template <class A>\nA identity(A _a) {\nreturn _a;\n}\n")
  (fact "defn (annotated)"
    (cpp '(defn ^int fna [] 0))
    => "int fna() {\nreturn 0;\n}\n")
  (fact "defn (annotated 2)"
    (cpp '(defn ^int fna [a b] (+ a b 0)))
    => "int fna(long _a, long _b) {\nreturn ((_a + _b) + 0);\n}\n")
  (fact "defn (annotated 3)"
    (cpp '(defn fnb [^int a ^char b]))
    => "void fnb(int _a, char _b) {\n}\n")    
  (fact "when"
    (cpp '(when d (println "hello") (println "world"))) 
    => "if (d) {\nprintln(\"hello\");\nprintln(\"world\");\n}")  
  (fact "when-not"
    (cpp '(when-not d (println "hello") (println "world"))) 
    => "if (!d) {\nprintln(\"hello\");\nprintln(\"world\");\n}")  
  ;(fact "cond"
  ;  (cpp '(cond a 1 b 2 c 3)) 
  ;  => "if (a) {\n1;\n} else if (b) {\n2;\n} else if (c) {\n3;\n}")  
  ;(fact "if-not"
  ;  (cpp '(if-not a b c)) => "if (!a) {\nb;\n} else {\nc;\n}")  
  (fact "and"
    (cpp '(and (< 3 4) (< -1.0 1.0))) => "if ((3 < 4)) {\n(-1.0 < 1.0);\n}"
    (cpp '(and (< 0 1) (< 1 2) (< 2 3))) => "if ((0 < 1)) {\nif ((1 < 2)) {\n(2 < 3);\n}\n}")  
  (fact "or"
    (cpp '(or (< 3 4) (< -1.0 1.0))) => "if ((3 < 4)) {\ntrue;\n} else {\n(-1.0 < 1.0);\n}"
    (cpp '(or (< 0 1) (< 1 2) (< 2 3))) 
    => "if ((0 < 1)) {\ntrue;\n} else if ((1 < 2)) {\ntrue;\n} else {\n(2 < 3);\n}")
  (fact "if and"
    (cpp '(if (and (< 3 4) (< 0 1)) (println 5)))
    => "bool _a;\nif ((3 < 4)) {\n_a = (0 < 1);\n}\nif (_a) {\nprintln(5);\n}")  
  (fact "if or"
    (cpp '(if (or (< 3 4) (< 0 1)) (println 5)))
    =>  "bool _a;\nif ((3 < 4)) {\n_a = true;\n} else {\n_a = (0 < 1);\n}\nif (_a) {\nprintln(5);\n}")  
   (comment (fact "if"
    (cpp '((if (< 0 1) inc dec) 5)) 
    => (str "std::function<long(long)> a__4219__auto__;\n"
            "if ((0 < 1)) {\na__4219__auto__ = inc;\n} else {\na__4219__auto__ = dec;\n}\n"
            "a__4219__auto__(5);")))  
   ;(fact "new"
   ; (cpp '(new Entity 3 (+ 1 2))) => "new Entity(3, (1 + 2))"
   ; (cpp '(Entity. 3 (+ 1 2))) => "new Entity(3, (1 + 2))")  
  (fact "eq"
    (cpp '(= 1 2)) => "(1 == 2)")   
  (fact "->"
    (cpp '(-> a (b 1) c)) => "c(b(a, 1))")  
  (fact "->>"
     (cpp '(->> a (b 1) c)) => "c(b(1, a))")  
  ;(fact "if-let"
  ;   (cpp '(if-let [a (< 1 2)] (println a))) => 'x)  
  (fact "when-let"
     (cpp '(when-let [a (< 1 2)] (println a))) 
     => "bool _a = (1 < 2);\nif (_a) {\nbool _b = _a;\nprintln(_b);\n}")  
  ;(let* [n__6055__auto__ 5] 
  ;  (loop* [i 0] 
  ;    (if (< i n__6055__auto__) 
  ;      (do (println i) (recur (inc i))))))  
  (fact "dotimes"
    (cpp '(dotimes [i 5] (println i))) 
    => (str "long _a = 5;\n"
            "long _b = 0;\n"
            "_c:\n"
            "if ((_b < _a)) {\n"
              "println(_b);\n"
              "_b = (_b - 1);\n"
              "goto _c;\n}"))  
  (fact "fn generic"
    (cpp '(fn [x] x)) =>  "[&](A _a) {\nreturn _a;\n}")  
  (fact "fn typed"
    (cpp '(fn [a] (+ a 1))) =>  "[&](long _a) {\nreturn (_a + 1);\n}")  
  (fact "fn"
    (cpp '(fn [a] (if (< a 2) (println 2)))) 
    =>  "[&](long _a) {\nif ((_a < 2)) {\nprintln(2);\n}\n}")  
  (facts "array"
    (cpp '(def doubles (array double 4))) =>  "const double* doubles = new double[4];"
    (cpp '(array double 4)) => "new double[4]")  
  (fact "loop"
    (cpp '(loop [x 4] x)) => "long _a = 4;\n_b:\n_a;"))

(facts "casts"       
  (fact "double"
    (cpp '(cast double (+ 1 2))) => "(double)((1 + 2))"
    (cpp '(def e (double 1))) => "const double e = (double)1;"))

(facts "structs"
  (fact "product"
    (cpp '(defstruct product (weight int) (price float))) 
    => "struct product {\nfloat price;\nint weight;\n};\n"))

(facts "delete"
  (cpp '(let [a (array double 4)]
          (delete a))) 
  => "double* _a = new double[4];\ndelete _a;\n")
     
(facts "math"
  (fact "plus"
    (cpp '(+ 1 2 3)) => "((1 + 2) + 3)")   
  (fact "complex"
    (cpp '(+ 1 2 (- 3 4 5))) => "((1 + 2) + ((3 - 4) - 5))")) 

(facts "escaping"
  (cpp 'empty?) => "empty_QMARK_"
  (cpp 'a->b) => "a__GT_b"
  (cpp 'a<>b) => "a_LT__GT_b")

(defn type2str
  [s]
  (->> s types/expand-type (type->string core-env)))

(facts "type to string"
  (type2str 'char.const*) => "const char*"
  (type2str 'char*.const) => "char* const"
  (type2str 'char.const*.const) => "const char* const")

(facts "examples"       
  (fact "multiplier"
    (cpp '(defn multiplier [factor]
            (fn [x] (* (+ x 0) factor))))    
    => (str "std::function<long(long)> multiplier(long _a) {\n"
            "return [&](long _b) {\nreturn ((_b + 0) * _a);\n};\n}\n"))  
  (fact "inline fn"
    (cpp '((fn [x] (+ x 1)) 1)) => "[&](long _a) {\nreturn (_a + 1);\n}(1)")   
  (fact "let over fn"
    (cpp '(def inc (let [x 1] (fn [y] (+ x y))))) 
    => (str "std::function<long(long)> _a;\n"
            "long _b = 1;\n"
            "_a = [&](long _c) {\nreturn (_b + _c);\n};\n"
            "const std::function<long(long)> inc = _a;")) 
  (fact "eq"
    (cpp '(defn eq [x y] (= x y))) 
    => "template <class A>\nbool eq(A _a, A _b) {\nreturn (_a == _b);\n}\n")  
  (fact "string"
    (cpp '(def greeting "Hello, world!")) => "const std::string greeting = \"Hello, world!\";")) 


