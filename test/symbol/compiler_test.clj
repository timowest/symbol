(ns symbol.compiler-test
  (:use symbol.compiler
        midje.sweet))

(facts "normalize"
  (normalize 'clojure.core/let*) => 'let*)

(defn expand
  [form]
  (expand-all (:macros core-forms) form))

(facts "let"
  (expand '(let [a 1 b 2] (+ a b))) => '(let* [a 1 b 2] (+ a b)))

(facts "defn"
  (expand '(defn identity [a] a)) => '(def identity (fn* ([a] a))))

(facts "when"
  (expand '(when a (println "hello") (println "world"))) 
          => '(if a (do (println "hello") (println "world"))))

(facts "cond"
  (expand '(cond a 1 b 2 c 3)) =>  '(if a 1 (if b 2 (if c 3 nil))))

(facts "if-not"
   (expand '(if-not a b c)) => '(if (not a) b c))

; and
; or

(facts "->"
   (expand '(-> a (b 1) c)) => '(c (b a 1)))

(facts "->>"
   (expand '(->> a (b 1) c)) => '(c (b 1 a)))

; if-let
; when-let

(facts "dotimes"
  (expand '(dotimes [i 5] (println i))) => anything)

(facts "fn"
  (expand '(fn [x] x)) => '(fn* ([x] x)))

(facts "loop"
  (expand '(loop [x 4] x)) => '(loop* [x 4] x))


   
   

