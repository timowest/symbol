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
  (:use [clojure.core.logic :exclude [membero]]
        [clojure.core.match :only (match)]
        symbol.common))

(declare typedo typeso last-typeo annotatedo geno)

(defne membero  
  [x l]
  ([_ [head . tail]]
    (conde
      ((== head x))
      ((membero x tail)))))

(defn mergeo
  [x y z]
  (fn [a]
    (let [gx (walk a x)
          gy (walk a y)]
      (unify a z (merge-with concat gx gy)))))

(defn geto
  [k v m]
  (fn [a]
    (let [gv (get (walk a m) (walk a k))
          f (first gv)]
      (if (and f (= (rest gv) []))
        (unify a v f)
        ((membero v gv) a)))))

(defn updateo
  [x k v y]
  (fn [a]
    (let [gx (walk a x)
          gk (walk a k)
          gv (walk a v)]
      (unify a y (update-in gx [gk] conj gv)))))

(defnu ifo ; (if c t e) (if c t)
  [env form new-env]
   ([_ ['if ?c ?t ?e] _]
     (fresh [type type1 type2 env2]
       (typeso env [?c ?t ?e] ['boolean type1 type2] env2)
       (matchu [type1 type2] ; allows one to be void typed
         ([type type])
         ([type 'void])
         (['void ype]))
       (updateo env2 form type new-env)))
   ([_ ['if ?c ?t] _]
     (fresh [type env2]
            (typeso env [?c ?t] ['boolean type] env2)
            (updateo env2 form type new-env))))

(defn ftypeo
  [a at]
  (conda
    ((annotatedo a at))
    (succeed)))

(defna ftypeso
  [env args types new-env]
  ([_ [?a . ?r] [?at . ?rt] _]
    (fresh [env2]
           (ftypeo ?a ?at)
           (ftypeso env ?r ?rt env2)
           (updateo env2 ?a ?at new-env)))
  ([?e [] [] ?e]))

(defnu fno ; (fn args body)
  [env form new-env]  
  ([_ ['fn* [?args . ?exprs]] _]
    (fresh [argst type env1 env2 env3]
           (ftypeso env ?args argst env2)
           (last-typeo env2 ?exprs type env3)
           (updateo env3 form ['fn argst type] new-env)))
  ([_ ['fn* ?name [?args . ?exprs]] _]
    (fresh [argst type env1 env2 env3 env4]
           (ftypeso env ?args argst env2)
           (updateo env2 ?name ['fn argst type] env3)
           (last-typeo env3 ?exprs type env4)
           (updateo env4 form ['fn argst type] new-env))))

(defnu bindingso
  [env bindings types new-env]
  ([_ [?k ?v . ?rest] [?vt . ?restt] _]
    (fresh [env2 env3]
           ;(typeso env [?v] [?vt] env2)
           (typedo env ?v env2)
           (geto ?v ?vt env2)
           (condu ((annotatedo env2 ?k env3)) 
                  ((updateo env2 ?k ?vt env3)))
           (bindingso env3 ?rest ?restt new-env)))
  ([?e [] [] ?e]))

(defna argso
  [bindings args]
  ([[?k ?v . ?rest] [?k . ?resta]]
    (argso ?rest ?resta))
  ([[] []]))                   

(defnu loopo ; (loop* name bindings body*)
  [env form new-env]
  ([_ ['loop* ?name ?bindings . ?exprs] _] 
    (fresh [types env2 env3 env4 type args]
           (bindingso env ?bindings types env2)
           (argso ?bindings args)
           (updateo env2 ?name ['loop args types type] env3)
           (last-typeo env3 ?exprs type env4)
           (updateo env4 form type new-env))))

(defnu recuro ; (recur f args*)
  [env form new-env]
  ([_ ['recur* ?f . ?args] _]
    (fresh [type args types env2]
           (geto ?f ['loop args types type] env)           
           (typeso env ?args types env2)
           (updateo env2 form 'void new-env))))

(defnu leto ; (let* bindings body*)
  [env form new-env]
  ([_ ['let* ?bindings . ?exprs] _] ; normal let
    (fresh [types env2 env3 type]
           (bindingso env ?bindings types env2)
           (last-typeo env2 ?exprs type env3)
           (updateo env3 form type new-env))))

(defnu applyo ; (f args*)
  [env form new-env]
  ([_ [?f . ?args] _] 
    (fresh [env2 template op types type env3]
           (typedo env ?f env2) 
           (geto ?f template env2)
           (geno template [op types type])
           (conda
             ((== op 'fn))
             ((== op 'sf)))
           (typeso env2 ?args types env3)
           (updateo env3 form type new-env))))
    
(defnu dot ; (. obj member args*)
  [env form new-env]
  ([_ [_ ?obj ?member . ?args] _]
    (fresh [env2 clazz members membert argst env3 type]
           (typedo env ?obj env2)
           (geto ?obj ['pointer clazz] env2)
           (geto clazz ['class clazz members] env)
           (geto ?member membert members)
           (typeso env2 ?args argst env3)
           (matcha [membert type]
                   ([['method argst type] type]) 
                   ([type type]))
           (updateo env3 form type new-env))))
                   
(defnu newo ; (new Class args*)
  [env form new-env]
  ([_ ['new ?class . ?args] _]
    (fresh [argst members env2]
           (typeso env ?args argst env2)
           (geto ?class ['class ?class members] env)
           (geto :new argst members)
           (updateo env2 form ['pointer ?class] new-env))))

(defnu defo  ; (def name expr)
  [env form new-env]
  ([_ ['def ?name ?expr] _]
    (fresh [env2 type env3]
           (updateo env ?name type env2)
           ;(typeso env2 [?expr] [type] env3)
           (typedo env2 ?expr env3)
           (geto ?expr type env3)
           (updateo env3 form type new-env)))
  ([_ ['def ?name] _]
    (fresh [type env2]
           (annotatedo env ?name env2)
           (geto ?name type env2)
           (updateo env2 form type new-env))))
      
(defnu doo ; (do exprs*)
  [env form new-env]
  ([_ ['do . ?exprs] _]
    (fresh [type env2]
           (last-typeo env ?exprs type env2)
           (updateo env2 form type new-env))))

(defn include*
  [i result]
  (fn [a]
    (let [gi (walk a i)
          content (if (= gi "iostream") ; FIXME iostream causes StackOverflowError
                     []
                    (includes/include gi))]
      (when content
        (unify a result content)))))    

(defna includeo
  [env form new-env]
  ([_ ['include ?f] _]
    (fresh [content nenv]
           (include* ?f content)
           (mergeo env content nenv)
           (updateo nenv form 'include new-env)))        
  ([_ ['include ?f . ?rest] _]
    (fresh [content new-form reste nenv]
           (include* ?f content)
           (conso 'include ?rest new-form)
           (includeo env new-form reste)
           (mergeo reste content nenv)
           (updateo nenv form 'include new-env)))
  ([?e ['include] ?e]))   

(defnu arrayo
  [env form new-env]
  ([_ ['array ?type ?dimensions] _]
    (updateo env form ['pointer ?type] new-env)))

(defnu structo
  [env form new-env]
  ([_ ['struct ?name . ?members] _]
    (updateo env form ['struct ?name ?members] new-env)))
     
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
      (unify a fresh (expand-type gtemplate)))))
                 
; TODO this should probably first check if symbol can be resolved via the env
;      and if not, take the symbol as such
(defn annotatedo 
  ([env form new-env]
    (fresh [type]
           (annotatedo form type)
           (updateo env form type new-env)))
  ([form type]
    (fn [a]
      (let [gf (walk a form)]
        (if-let [t (-> gf meta :tag)]
          (unify a type (expand-type t)))))))

(defn literalo
  ([env form new-env]
    (fresh [type]
           (literalo form type)
           (updateo env form type new-env)))
  ([form type]
    (fn [a]
      (let [gf (walk a form)]
        (if-let [t (literal-types (.getClass gf))]
          (unify a type t)))))) 

(defn failo
  [form]
  (fn [a]
    (throw (IllegalStateException. 
             (str "Type inference failed for " (walk a form))))))

(defna last-typeo
  [env args last new-env]
  ([_ [?a] _ _]
    (typedo env ?a new-env)
    (geto ?a last new-env))    
  ([_ [?a . ?rest] _ _]
    (fresh [env2]
           (typedo env ?a env2)
           (last-typeo env2 ?rest last new-env)))) 
     
(defna typeso
  [env args types new-env]
  ([_ [?a . ?r] [?at . ?rt] _]
    (fresh [env2]
           (typedo env ?a env2)
           (geto ?a ?at env2)           
           (typeso env2 ?r ?rt new-env)))
  ([?e [] [] ?e]))

; TODO special handling of sets, maps and vectors?
(defnu typedo
  [env form new-env]
  ([_ nil _] (== env new-env))
  ([?e ['ns* ?name] ?e])
  ([?e ['comment . _] ?e])
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
  ([_ _ _] (condu ((fresh [type] 
                         (geto form type env)
                         (== env new-env)))
                  ((annotatedo env form new-env))
                  ((literalo env form new-env))))) 

(defn new-env 
  [env form]
  (first (run 1 [q] (typedo env form q))))

(defn type-and-env
  [env form]
  (first
    (run 1 [type env2]
         (typedo env form env2)
         (geto form type env2))))

(defn typeof
  ([form]
    (typeof {} form))
  ([env form]
    (first
      (run 1 [type] 
           (fresh [env2]
                  (typedo env form env2)
                  (geto form type env2))))))