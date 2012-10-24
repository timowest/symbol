(ns symbol.analysis
  (:require [clojure.walk :as walk]))

(defn fn-names
  [form]
  (let [args (nth form (if (symbol? (second form)) 2 1))
        mapped (zipmap args (repeatedly gensym))]
    (walk/postwalk-replace mapped form)))
  
(defn let-names
  [form]
  (let [bindings (second form)
        args (map first (partition 2 bindings))
        mapped (zipmap args (repeatedly gensym))]
    (walk/postwalk-replace mapped form)))
 
(defn loop-names
  [form]
  (let [bindings (nth form 2)
        args (map first (partition 2 bindings))
        mapped (zipmap args (repeatedly gensym))]
    (walk/postwalk-replace mapped form)))

(defn form?
  [form s]
  (and (seq? form) (= (first form) s)))

(defn unique-names
  "Replaces local names in fn* and let* forms with unique ones"
  [form]
  (walk/postwalk
    (fn [f]
      (cond (form? f 'fn*) (fn-names f)
            (form? f 'let*) (let-names f)
            (form? f 'loop*) (loop-names f)
            :else f))
    form))

(defn expand-recur
  [form s]
  (->> (walk/postwalk
        (fn [f]
          (if (form? f 'recur)
            (concat ['recur* s] (rest f))
            f))
        form)
    rest
    (concat ['loop* s])))

(defn expand-loop
  "Add symbols to recur and loops."
  [form]
  (walk/postwalk
    (fn [f]
      (cond (form? f 'loop*) (expand-recur f (gensym))
            :else f))
    form))

; if fn* let* . new def do

(defmulti simple first)

(defn complex? 
  [form]
  ('#{if let* do} (first form)))

(defmethod simple 'if
  [form])

(defmethod simple :default
  [form])

(defn simplify
  [form]
  (walk/postwalk
    (fn [f]
      (if (list? f)
        (simple f)
        f))
    form))