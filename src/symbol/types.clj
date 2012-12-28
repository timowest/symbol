;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.types
  (:refer-clojure :exclude [== reify inc type])
  (:require [clojure.walk :as walk]
            [symbol.includes :as includes])
  (:use clojure.core.logic
        symbol.common))

(defmacro time2
  [text expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (str ~text " elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))

(declare typedo typeso annotatedo geno)

(def specials '#{if fn* let* . new def do})

(defn lasto
  "A relation where l is a collection, such that a is the last of l"
  [l a]
  (fresh [begin d]
    (appendo begin [a] l)))

(defne ifo ; (if c t e) (if c t)
  [env form new-env]
   ([_ ['if ?c ?t ?e] [[form ?type] . ?env2]]
     (fresh [type1 type2]
       (typeso env [?c ?t ?e] ['boolean type1 type2] ?env2)
       (matcha [type1 type2] ; allows one to be void typed
         ([?type ?type])
         ([?type 'void])
         (['void ?type]))))
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
  ([_ ['fn* [?args . ?exprs]] [[form ['fn ?argst ?type]] . ?env3]]
    (fresh [env1 env2 exprst]
           (ftypeso env ?args ?argst env2)
           (typeso env2 ?exprs exprst ?env3)
           (lasto exprst ?type)))
  ([_ ['fn* ?name [?args . ?exprs]] [[form ['fn ?argst ?type]] . ?env4]]
    (fresh [env1 env2 env3 exprst]
           (ftypeso env ?args ?argst env2)
           (conso [?name ['fn ?argst ?type]] env2 env3)
           (typeso env3 ?exprs exprst ?env4)
           (lasto exprst ?type))))

(defne bindingso
  [env bindings types new-env]
  ([_ [?k ?v . ?rest] [?vt . ?restt] _]
    (fresh [env2 env3]
           (typeso env [?v] [?vt] env2)
           (conda ((annotatedo env2 ?k env3)) 
                  ((conso [?k ?vt] env2 env3)))
           (bindingso env3 ?rest ?restt new-env)))
  ([?e [] [] ?e]))

(defne argso
  [bindings args]
  ([[?k ?v . ?rest] [?k . ?resta]]
    (argso ?rest ?resta))
  ([[] []]))                   

(defne loopo ; (loop* name bindings body*)
  [env form new-env]
  ([_ ['loop* ?name ?bindings . ?exprs] _] 
    (fresh [types env2 env3 env4 exprst type args]
           (bindingso env ?bindings types env2)
           (argso ?bindings args)
           (conso [?name ['loop args types type]] env2 env3)
           (typeso env3 ?exprs exprst env4)
           (lasto exprst type)
           (conso [form type] env4 new-env))))

(defne recuro ; (recur f args*)
  [env form new-env]
  ([_ ['recur* ?f . ?args] [[form 'void] . ?env2]]
    (fresh [type args types]
           (membero [?f ['loop args types type]] env)
           (typeso env ?args types ?env2))))

(defne leto ; (let* bindings body*)
  [env form new-env]
  ([_ ['let* ?bindings . ?exprs] _] ; normal let
    (fresh [types env2 exprst env3 type]
           (bindingso env ?bindings types env2)
           (typeso env2 ?exprs exprst env3)
           (lasto exprst type)
           (conso [form type] env3 new-env))))

(defne applyo ; (f args*)
  [env form new-env]
  ([_ [?f . ?args] [[form ?type] . ?env3]] 
    (fresh [env2 template op types]
           (typedo env ?f env2) 
           (membero [?f template] env2)
           (geno template [op types ?type])
           (membero op ['fn 'sf])
           (typeso env2 ?args types ?env3))))
    
(defne dot ; (. obj member args*)
  [env form new-env]
  ([_ [_ ?obj ?member . ?args] [[form ?type] . ?env3]]
    (fresh [env2 clazz members membert argst]
           (typedo env ?obj env2)
           (membero [?obj ['pointer clazz]] env2)
           (membero [clazz ['class clazz members]] env)
           (membero [?member membert] members)
           (typeso env2 ?args argst ?env3)
           (matcha [membert ?type]
                   ([['method argst ?type] ?type]) 
                   ([?type ?type])))))
                   
(defne newo ; (new Class args*)
  [env form new-env]
  ([_ ['new ?class . ?args] [[form ['pointer ?class]] . ?env2]]
    (fresh [argst members]
           (typeso env ?args argst ?env2)
           (membero [?class ['class ?class members]] env)
           (membero [:new argst] members))))

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

(defn include*
  [i result]
  (fn [a]
    (let [gi (walk a i)
          content (if (= gi "iostream") ; FIXME iostream causes StackOverflowError
                     []
                    (includes/include gi))]
      (when content
        (unify a [result] [content])))))    

(defne includeo
  [env form new-env]
  ([_ ['include ?f . ?rest] [[form 'include] . ?nenv]]
    (fresh [content new-form reste]
           (include* ?f content)
           (conso 'include ?rest new-form)
           (includeo env new-form reste)
           (appendo content reste ?nenv)))
  ([?e ['include] ?e]))   

(defne arrayo
  [env form new-env]
  ([_ ['array ?type ?dimensions] [[form ['pointer ?type]] . env]]))

(defne structo
  [env form new-env]
  ([_ ['struct ?name . ?members] [[form ['struct ?name ?members]] . env]]))
     
(def expandables
  (concat 
    '(A B C D E F G H I J K L M N O P Q R X Y Z)
    (map #(symbol (str "_" %)) (range 0 26))))

(defn expand-type
  [type]
  (if (coll? type)
    (walk/postwalk-replace 
      (zipmap expandables (repeatedly lvar))
      type)
    type))

(defn geno
  [template fresh]
  (fn [a]    
    (let [gtemplate (walk a template)]
      (unify a [fresh] [(expand-type gtemplate)]))))
                 
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
  ([_ nil _] (== env new-env))
  ([_ ['ns* ?name] _] (== env new-env))
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
  ([_ ['include . _] _] (includeo env form new-env))
  ([_ ['array . _] _] (arrayo env form new-env))
  ([_ ['struct . _] _] (structo env form new-env))
  ([_ _ _] (conda ((fresh [type]
                         (membero [form type] env) 
                         (== env new-env)))
                  ((annotatedo env form new-env))
                  ((literalo env form new-env))))) 

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