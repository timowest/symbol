(ns symbol.analysis-test
  (:use symbol.analysis
        midje.sweet))

(facts "unique names for let* bindings"  
  (unique-names '(let* [a 1 b 2] (+ a b))) => '(let* [x 1 x 2] (+ x x))
  (provided (gensym) => 'x))
  
(facts "unique names for loop*"
  (unique-names '(loop* named [a 1 b 2] (+ a b))) => '(loop* named [x 1 x 2] (+ x x))
  (provided (gensym) => 'x))
    
(facts "unique names for fn* args"
  (unique-names '(fn* named [a b c] (+ a b c))) => '(fn* named [x x x] (+ x x x))
  (provided (gensym) => 'x)
  (unique-names '(fn* [a b c] (+ a b c))) => '(fn* [x x x] (+ x x x))
  (provided (gensym) => 'x))

(facts "expand recur"
  (expand-recur '(loop* [i 0] (when (< i 5) (println i) (recur (inc i))))  'x)
  => '(loop* x [i 0] (when (< i 5) (println i) (recur* x (inc i)))))

(facts "expand loop"
  (expand-loop '(loop* [i 0] (when (< i 5) (println i) (recur (inc i)))))
  => '(loop* x [i 0] (when (< i 5) (println i) (recur* x (inc i))))
  (provided (gensym) => 'x))
