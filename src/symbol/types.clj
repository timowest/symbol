(ns symbol.types
  (:refer-clojure :exclude [== reify inc type])
  (:use [clojure.core.logic]))

(declare typedo typeso)

; TODO convert uppercase letters to lvars
(def ^:private env 
  '((set!  (fn [A A] void))
    (pset! (fn [(pointer A) A] void))
    (pset! (fn [(pointer A) long A] void))
    (pref  (fn [(pointer A long)] A))
    (+     (fn [A A] A))
    (-     (fn [A A] A))
    (*     (fn [A A] A))
    (/     (fn [A A] A))))

(defne ifo ; (if c t e) (if c t)
  [env form type]
  ([_ ['if ?c ?t ?e] _] 
    (typedo env ?t type) 
    (typedo env ?e type))
  ([_ ['if ?c ?t] _] (typedo env ?t type)))

(defne fno ; (fn args body)
  [env form type]
  ([_ ['fn [?arg . ?rest] . ?body] ['fn [?argt . ?argst] ?type]]
    (fresh [new-env new-fn]
           (conso [?arg ?argt] env new-env)
           (appendo ['fn ?rest] ?body new-fn)
           (fno new-env new-fn ['fn ?argst ?type])))
  ([_ ['fn [] ?stmt . _] ['fn [] ?type]] ; TODO should be last stmt
    (typedo env ?stmt ?type)))
                              
(defne leto ; (let* bindings body*)
  [env form type]
  ([_ ['let* [?k ?v . ?rest] . ?body] _]
    (fresh [vtype new-env new-let]
           (typedo env ?v vtype)
           (conso [?k vtype] env new-env)
           (appendo ['let* ?rest] ?body new-let)
           (leto new-env new-let type)))
  ([_ ['let* [] ?stmt . _] _] ; TODO should be last stmt
    (typedo env ?stmt type)))

(defne applyo ; (f args*)
  [env form type]
  ([_ [?f . ?args] _]
    (fresh [ftype types]
           (typedo env ?f ftype)
           (typeso env ?args types)
           (== ftype ['fn types type]))))

(defne dot ; (. obj member args*)
  [env form type]
  ([_ [_ ?obj ?member . ?args] _]
    (fresh [members membert argst]
           (typedo env ?obj ['object members])
           (typedo members ?member membert) 
           (typeso env ?args argst)
           (matcha [membert type]
                   ([['fn argst type] type]) 
                   ([type type])))))

(defne newo ; (new Class args*)
  [env form type]
  ([_ ['new ?class . ?args] ['object ?members]]
    (fresh [argst]
           (typedo env ?class ['class ?members])
           (typeso env ?args argst)
           (membero [:new argst] ?members))))
            
(def ^:private literal-types
  {Long      'long
   Double    'double
   String    'string
   Character 'char
   clojure.lang.Ratio     'ratio})
    
(defn literalo
  [form type]
  (fn [a]
    (let [gf (walk a form)]
      (if-let [t (literal-types (.getClass gf))]
        (unify a [type] [t])))))

(defne typeso 
  [env args types]
  ([_ [?a . ?r] [?at . ?rt]] 
    (typedo env ?a ?at)
    (typeso env ?r ?rt))
  ([_ [] []]))
          

; TODO special handling of sets, maps and vectors?
; TODO def loop recur new set!
(defnu typedo
  [env form type]
  ([_ ['if . _] _] (ifo env form type))
  ([_ ['fn . _] _] (fno env form type))
  ([_ ['let* . _] _] (leto env form type))
  ([_ [?dot . _] _] (== ?dot '.) (dot env form type))
  ([_ ['new . _] _] (newo env form type))
  ([_ [?fn . _] _] (applyo env form type))  
  ([_ _ _] (conda ((membero [form type] env))
                  ((literalo form type))))) ; TODO use typeo only for simple types
