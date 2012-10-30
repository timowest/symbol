(ns symbol.analysis
  (:require [clojure.walk :as walk])
  (:use symbol.util))

(declare unique-names expand-recur simplify)

(defn postwalk
  [form pred f]
  (walk/postwalk
    (fn [arg]
      (if (pred arg)
        (f arg)
        arg))
    form))

(defn- replace-names
  [form names]
  (let [mapped (zipmap names (repeatedly gensym))]
    (walk/postwalk-replace mapped form)))

(defn fn-names
  [form]
  (let [args (first (nth form (if (symbol? (second form)) 2 1)))]
    (replace-names form args)))
  
(defn let-names
  [form]
  (let [bindings (second form)
        args (map first (partition 2 bindings))]
    (replace-names form args)))
 
(defn loop-names
  [form]
  (let [bindings (nth form 2)
        args (map first (partition 2 bindings))]
    (replace-names form args)))

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
  (->> (postwalk form 
                 #(form? % 'recur) 
                 #(concat ['recur* s] (rest %))) 
    rest
    (concat ['loop* s])))

(defn expand-loop
  "Add symbols to recur and loops."
  [form]
  (postwalk form 
            #(form? % 'loop*) 
            #(expand-recur % (gensym))))

; if fn* let* loop* . new def do

(defn wrap
  [form args]
  (let [forms (filter seq? args)
        mapped (zipmap forms (repeatedly gensym))
        walked (walk/postwalk-replace mapped form)
        bindings (vec (mapcat (juxt mapped identity) forms))]
    `(let* ~bindings ~walked)))

(defmulti simple first)

(defmethod simple 'if
  [[_ c & r :as form]]
  (if (complex? c)
    (let [s (gensym)] 
      `(let* [~s ~c] (if ~s ~@r)))
    form))

(defmethod simple 'fn* 
  [form]
  form)

(defmethod simple 'let*
  [[_ bindings & body :as form]]
  (if (and (= (count body) 1)
           (form? (first body) 'let*)) 
    (let [[_ bindings2 & body] (first body)
          bindings (vec (concat bindings bindings2))]
      `(let* ~bindings ~@body))
    form))

(defmethod simple 'loop*
  [form]
  form)

(defmethod simple 'new ; (new Class args*)
  [[new clazz & args :as form]]
   (if (some complex? args)
     (wrap form args)
     form))

(defmethod simple '. ; (. obj member args*)
  [[_ obj member & args :as form]]
  (if (some complex? args)
    (wrap form args)
    form))  

(defmethod simple 'do
  [[_ f & r :as form]]
  (if (seq r) 
    form
    f))

(defmethod simple :default ; apply
  [[_ & args :as form]]
  (if (some complex? args)
    (wrap form args)
    form))
      
(defn simplify
  [form]
  (postwalk form seq? simple))

(def convert (comp simplify unique-names expand-loop))