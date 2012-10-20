(ns symbol.analysis
  (:require [clojure.walk :as walk]))

(defn fn-names
  [form]
  (let [args (nth form (if (symbol? (second form)) 2 1))
        mapped (zipmap args (repeatedly gensym))]
    (walk/postwalk-replace mapped form)))
  
(defn let-names
  [form]
  (let [bindings (nth form (if (symbol? (second form)) 2 1))
        args (map first (partition 2 bindings))
        mapped (zipmap args (repeatedly gensym))]
    (walk/postwalk-replace mapped form)))
 
(defn unique-names
  [form]
  (walk/postwalk
    (fn [f]
      (cond (and (seq? f) (= (first f) 'fn*)) (fn-names f)
            (and (seq? f) (= (first f) 'let*)) (let-names f)
            :else f))
    form))

; if fn* let* . new def do

(defn as-stmt
  [form]
  (with-meta form (assoc (meta form) :stmt true)))  

(defn stmt?
  [form]
  (and (seq? form) (-> form meta :stmt)))

(defn analyze-form
  [form]
  (let [f (first form)
        stmt (cond (= f 'if) (some stmt? (rest form))
                   (= f 'fn*) false
                   (= f 'let*) true
                   (= f '.) (some stmt? (rest form))
                   (= f 'new) (some stmt? (drop 2 form))
                   (= f 'def) (some stmt? (rest form))
                   (= f 'do) (if (= (count form) 2) 
                               (stmt? second)
                               true)                               
                   :else (some stmt? (rest form)))]
    (if stmt
      (as-stmt form)
      form)))

(defn analyze
  "Associates form with :stmt true metadata if it needs to be expressed 
   vi a stmt."
  [form]
  (walk/postwalk 
    (fn [f]
      (if (list? f) 
        (analyze-form f)
        f))        
    form))