(ns symbol.types
  (:refer-clojure :exclude [== reify inc type])
  (:require [clojure.walk :as walk])
  (:use [clojure.core.logic]))

(declare typedo typeso annotatedo)

(defn lasto
  "A relation where l is a collection, such that a is the last of l"
  [l a]
  (fresh [begin d]
    (appendo begin [a] l)))

; TODO convert uppercase letters to lvars
(def ^:private env 
  '((set!  (fn [A A] void))
    (pset! (fn [(pointer A) A] void))
    (pset! (fn [(pointer A) long A] void))
    (pref  (fn [(pointer A long)] A))
    (<     (fn [A A] boolean))
    (>     (fn [A A] boolean))
    (<=    (fn [A A] boolean))
    (>=    (fn [A A] boolean))
    (+     (fn [A A] A))
    (-     (fn [A A] A))
    (*     (fn [A A] A))
    (/     (fn [A A] A))))
  
(defne ifo ; (if c t e) (if c t)
  [env form new-env]
   ([_ ['if ?c ?t ?e] [[form ?type] . ?env2]]
     (typeso env [?c ?t ?e] ['boolean ?type ?type] ?env2))
   ([_ ['if ?c ?t] [[form ?type] . ?env2]]
     (typeso env [?c ?t] ['boolean ?type] ?env2)))

(defne ftypeso
  [env args types new-env]
  ([_ [?a . ?r] [?at . ?rt] [[?a ?at] . ?env2]]
    (conda 
      ((annotatedo ?a ?at))
      ((== ?at ?at)))
    (ftypeso env ?r ?rt ?env2))
  ([?e [] [] ?e]))

(defne fno ; (fn args body)
  [env form new-env]
  ([_ ['fn ?args . ?stmts] [[form ['fn ?argst ?type]] . ?env3]]
    (fresh [env1 env2 stmtst]
           (ftypeso env ?args ?argst env2)
           (typeso env2 ?stmts stmtst ?env3)
           (lasto stmtst ?type))))

; XXX ignores annotated keys
(defne bindingso
  [env bindings types new-env]
  ([_ [?k ?v . ?rest] [?vt . ?restt] _]
    (fresh [env2 env3]
           (typeso env [?v] [?vt] env2)
           (conso [?k ?vt] env2 env3)
           (bindingso env3 ?rest ?restt new-env)))
  ([?e [] [] ?e]))

(defne leto ; (let* bindings body*)
  [env form new-env]
  ([_ ['let* ?name ?bindings . ?stmts] _] ; named let
    (fresh [types env2 env3 env4 stmtst type]
           (bindingso env ?bindings types env2)
           (conso [?name ['fn types type]] env2 env3)
           (typeso env3 ?stmts stmtst env4)
           (lasto stmtst type)
           (conso [form type] env4 new-env)))
  ([_ ['let* ?bindings . ?stmts] _] ; normal let
    (fresh [types env2 stmtst env3 type]
           (bindingso env ?bindings types env2)
           (typeso env2 ?stmts stmtst env3)
           (lasto stmtst type)
           (conso [form type] env3 new-env))))

(defne applyo ; (f args*)
  [env form new-env]
  ([_ [?f . ?args] _]
    (fresh [types type env2]
           (membero [?f ['fn types type]] env)
           (typeso env ?args types env2)
           (conso [form type] env2 new-env))))
    
(defne dot ; (. obj member args*)
  [env form new-env]
  ([_ [_ ?obj ?member . ?args] _]
    (fresh [env2 members membert argst env3 type]
           (typedo env ?obj env2)
           (membero [?obj ['object members]] env2)
           (membero [?member membert] members)
           (typeso env2 ?args argst env3)
           (matcha [membert type]
                   ([['fn argst type] type]) 
                   ([type type]))
           (conso [form type] env3 new-env))))
                   
(defne newo ; (new Class args*)
  [env form new-env]
  ([_ ['new ?class . ?args] _]
    (fresh [members argst env2]
           (typeso env ?args argst env2)
           (membero [?class ['class members]] env)
           (membero [:new argst] members)
           (conso [form ['object members]] env2 new-env))))

(defne defo  ; (def name expr)
  [env form new-env]
  ([_ ['def ?name ?expr] _]
    (fresh [env2 type]
           (typeso env [?expr] [type] env2)
           (conso [?name type] env2 new-env)))
  ([_ ['def ?name] _]
    (annotatedo env ?name new-env)))
                               
(def ^:private literal-types
  {Long      'long
   Double    'double
   String    'string
   Character 'char
   Boolean   'boolean
   clojure.lang.Ratio     'ratio})
   

(defn expand-type
  [type]
  (if (seq? type)
    (walk/postwalk-replace 
      (zipmap '(A B C D E F G H) (repeatedly lvar))
      type)
    type))
        
; TODO this should probably first check if symbol can be resolved via the env
;      and if not, take the symbol as such
(defn annotatedo 
  ([env form new-env]
    (fresh [type]
           (annotatedo form type)
           (conso [form type] env new-env)))
  ([form type]
  (fn [a]
    (let [gf (walk a form)]
      (if-let [t (-> gf meta :tag)]
        (unify a [type] [(expand-type t)]))))))

(defn literalo
  ([env form new-env]
    (fresh [type]
           (literalo form type)
           (conso [form type] env new-env)))
  ([form type]
  (fn [a]
    (let [gf (walk a form)]
      (if-let [t (literal-types (.getClass gf))]
        (unify a [type] [t]))))))

; TODO env args types new-env

(defne typeso
  [env args types new-env]
  ([_ [?a . ?r] [?at . ?rt] _]
    (fresh [env2]
           (typedo env ?a env2)
           (membero [?a ?at] env2)
           (typeso env2 ?r ?rt new-env)))
  ([?e [] [] ?e]))

; TODO special handling of sets, maps and vectors?
(defnu typedo
  [env form new-env]
  ([_ ['if . _] _] (ifo env form new-env))
  ([_ ['fn . _] _] (fno env form new-env))
  ([_ ['let* . _] _] (leto env form new-env))
  ([_ [?dot . _] _] (== ?dot '.) (dot env form new-env))
  ([_ ['new . _] _] (newo env form new-env))
  ([_ ['def . _] _] (defo env form new-env))
  ([_ [?fn . _] _] (applyo env form new-env))    
  ([_ _ _] (conda ((fresh [type]
                         (membero [form type] env) 
                         (== env new-env)))
                  ((annotatedo env form new-env))
                  ((literalo env form new-env))))) 

(defn typeof
  [env form]
  (first (run* [type]
               (fresh [env2]
                      (typedo env form env2)
                      (membero [form type] env2)))))
  


