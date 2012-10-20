(ns symbol.transform-test
  (:use symbol.transform
        midje.sweet))

(facts "unique names for let* bindings"
  (unique-names '(let* named [a 1 b 2] (+ a b))) => anything
  (unique-names '(let* [a 1 b 2] (+ a b))) => anything)       
    
(facts "unique names for fn* args"
  (unique-names '(fn* named [a b c] (+ a b c))) => anything
  (unique-names '(fn* [a b c] (+ a b c))) => anything)