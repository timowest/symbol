(ns symbol.analysis
  (:require [clojure.walk :as walk]))

; (-> form unique-names expand-recur simplify)

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

; if fn* let* loop* . new def do

(defn complex? 
  [form]
  (and (seq? form)
       ('#{if let* loop* do} (first form))))

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
  (walk/postwalk
    (fn [f]
      (if (list? f)
        (simple f)
        f))
    form))