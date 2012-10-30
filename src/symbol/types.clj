(ns symbol.types
  (:refer-clojure :exclude [== reify inc type])
  (:require [clojure.walk :as walk])
  (:use clojure.core.logic
        symbol.util))

(declare typedo typeso annotatedo)

(def specials '#{if fn* let* . new def do})

(defn lasto
  "A relation where l is a collection, such that a is the last of l"
  [l a]
  (fresh [begin d]
    (appendo begin [a] l)))

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
  ([_ ['fn* [?args . ?stmts]] [[form ['fn ?argst ?type]] . ?env3]]
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

(defne loopo ; (loop* name bindings body*)
  [env form new-env]
  ([_ ['loop* ?name ?bindings . ?stmts] _] 
    (fresh [types env2 env3 env4 stmtst type]
           (bindingso env ?bindings types env2)
           (conso [?name ['fn types type]] env2 env3)
           (typeso env3 ?stmts stmtst env4)
           (lasto stmtst type)
           (conso [form type] env4 new-env))))

(defne recuro ; (recur f args*)
  [env form new-env]
  ([_ ['recur* ?f . ?args] [[form ?type] . ?env2]]
    (fresh [types]
           (membero [?f ['fn types ?type]] env)
           (typeso env ?args types ?env2))))

(defne leto ; (let* bindings body*)
  [env form new-env]
  ([_ ['let* ?bindings . ?stmts] _] ; normal let
    (fresh [types env2 stmtst env3 type]
           (bindingso env ?bindings types env2)
           (typeso env2 ?stmts stmtst env3)
           (lasto stmtst type)
           (conso [form type] env3 new-env))))

(defne applyo ; (f args*)
  [env form new-env]
  ([_ [?f . ?args] [[form ?type] . ?env2]]
    (fresh [types]
           (membero [?f ['fn types ?type]] env)
           (typeso env ?args types ?env2))))
    
(defne dot ; (. obj member args*)
  [env form new-env]
  ([_ [_ ?obj ?member . ?args] [[form ?type] . ?env3]]
    (fresh [env2 members membert argst]
           (typedo env ?obj env2)
           (membero [?obj ['object members]] env2)
           (membero [?member membert] members)
           (typeso env2 ?args argst ?env3)
           (matcha [membert ?type]
                   ([['fn argst ?type] ?type]) 
                   ([?type ?type])))))
                   
(defne newo ; (new Class args*)
  [env form new-env]
  ([_ ['new ?class . ?args] [[form ['object ?members]] . ?env2]]
    (fresh [argst]
           (typeso env ?args argst ?env2)
           (membero [?class ['class ?members]] env)
           (membero [:new argst] ?members))))

(defne defo  ; (def name expr)
  [env form new-env]
  ([_ ['def ?name ?expr] [[form ?type] . ?env3]]
    (fresh [env2]
           (conso [?name ?type] env env2)
           (typeso env2 [?expr] [?type] ?env3)))
  ([_ ['def ?name] [[form ?type] . ?env2]]
    (annotatedo env ?name ?env2)
    (membero [?name ?type] ?env2)))
  
(defne doo ; (do exprs*)
  [env form new-env]
  ([_ ['do . ?exprs] [[form ?type] . ?env2]]
    (fresh [types]
           (typeso env ?exprs types ?env2)
           (lasto types ?type))))           

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
  ([_ ['fn* . _] _] (fno env form new-env))
  ([_ ['let* . _] _] (leto env form new-env))
  ([_ ['loop* . _] _] (loopo env form new-env))
  ([_ ['recur* . _] _] (recuro env form new-env))
  ([_ [?dot . _] _] (== ?dot '.) (dot env form new-env))
  ([_ ['new . _] _] (newo env form new-env))
  ([_ ['def . _] _] (defo env form new-env))
  ([_ ['do . _] _] (doo env form new-env))
  ([_ [?fn . _] _] (applyo env form new-env))    
  ([_ _ _] (conda ((fresh [type]
                         (membero [form type] env) 
                         (== env new-env)))
                  ((annotatedo env form new-env))
                  ((literalo env form new-env))))) 

(defn to-env
  [env]
  (for [[k v] (seq env)]
      [k (expand-type v)]))    

(defn new-env
  [env form]
  (first (run* [q] (typedo env form q))))

(defn type-and-env
  [env form]
  (->> (all
         (typedo env form env2)
         (membero [form type] env2))
    (run* [type env2])
    first))

(defn typeof
  ([form]
    (typeof [] form))
  ([env form]
    (->> (fresh [env2]
                (typedo env form env2)
                (membero [form type] env2))
      (run* [type])
      first)))