(ns symbol.transform
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
  (walk/prewalk
    (fn [f]
      (cond (and (seq? f) (= (first f) 'fn*)) (fn-names f)
            (and (seq? f) (= (first f) 'let*)) (let-names f)
            :else f))
    form))