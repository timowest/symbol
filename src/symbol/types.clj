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
        symbol.common))

(declare typedo typeso last-typeo annotatedo geno expand-type)

; optimization of walk-term for type environments
(extend-protocol IWalkTerm  
  
  ; TODO skip walking if forms have walked flag?
    
   clojure.lang.IPersistentMap
  (walk-term [v f]
    (with-meta
        (loop [v v r (transient {})]
          (if (seq v)
            (let [[vfk vfv] (first v)]
              (recur (next v) (assoc! r vfk ; skipped walking keys
                                      (walk-term (f vfv) f))))
            (persistent! r)))
        (meta v))))

(defne membero  
  [x l]
  ([_ [head . tail]]
    (conde
      ((== head x))
      ((membero x tail)))))

(def combine (partial merge-with concat))

(defn mergeo
  [x y z]
  (fn [a]
    (let [gx (walk a x)
          gy (walk a y)]
      (unify a z (combine gx gy)))))

(defn geto
  [k v m]
  (fn [a]
    (let [gv (get (walk a m) (walk a k))
          f (first gv)]
      (if (and f (empty? (rest gv)))
        (unify a v f)
        ((membero v gv) a)))))

(defn updateo
  [x k v y]
  (fn [a]
    (let [gx (walk a x)
          gk (walk a k)
          gv (walk a v)]
      (unify a y (update-in gx [gk] conj gv)))))

(defn assoco
  [x k v y]
  (fn [a]
    (let [gx (walk a x)
          gk (walk a k)
          gv (walk a v)]
      (unify a y (assoc gx gk (vector gv))))))

(defnu ifo ; (if c t e) (if c t)
  [env form type new-env]
   ([_ [_ ?c ?t ?e] _ _]
     (fresh [type1 type2 env2 env3 env4]
            ;(typeso env [?c ?t ?e] ['boolean type1 type2] env2)
            (typedo env ?c 'boolean env2)
            (typedo env2 ?t type1 env3)
            (typedo env3 ?e type2 env4)
            (matchu [type1 type2] ; allows one to be void typed
                    ([type type])
                    ([type 'void])
                    (['void type]))
            (assoco env4 form type new-env)))
   ([_ [_ ?c ?t] _ _]
     (fresh [env2 env3]
            ;(typeso env [?c ?t] ['boolean type] env2)
            (typedo env ?c 'boolean env2)
            (typedo env2 ?t type env3)
            (assoco env3 form type new-env))))

(defn ftype
  [arg]
  (if-let [t (-> arg meta :tag)]
    (expand-type t)
    (lvar)))

(defn ftypeso
  [env args types new-env]
  (fn [a]
    (let [genv (walk a env)
          gargs (walk a args)
          gtypes (map ftype gargs)
          gnenv (merge genv 
                       (zipmap gargs (map vector gtypes)))]
      (unify a [types new-env] [gtypes gnenv]))))
      

(defnu fno ; (fn args body)
  [env form type new-env]  
  ([_ [_ [?args . ?exprs]] _ _]
    (fresh [argst env1 env2 env3 rtype]
           (ftypeso env ?args argst env2)
           (last-typeo env2 ?exprs rtype env3)
           (== type ['fn argst rtype])
           (assoco env3 form type new-env)))
  ([_ [_ ?name [?args . ?exprs]] _ _]
    (fresh [argst env1 env2 env3 env4 rtype]
           (ftypeso env ?args argst env2)
           (== type ['fn argst rtype])
           (updateo env2 ?name type env3)
           (last-typeo env3 ?exprs rtype env4)            
           (assoco env4 form type new-env))))

(defnu bindingso
  [env bindings types new-env]
  ([_ [?k ?v . ?rest] [?t . ?restt] _]
    (fresh [env2 env3 vt]
           (typedo env ?v vt env2)
           (condu ((annotatedo env2 ?k ?t env3)) 
                  ((== ?t vt) (updateo env2 ?k ?t env3)))
           (bindingso env3 ?rest ?restt new-env)))
  ([?e [] [] ?e]))

(defn argso
  [bindings args]
  (fn [a]
    (let [gbindings (walk a bindings)]
      (unify a args (take-nth 2 gbindings)))))
  
(defnu loopo ; (loop* name bindings body*)
  [env form type new-env]
  ([_ [_ ?name ?bindings . ?exprs] _ _] 
    (fresh [types env2 env3 env4 args]
           (bindingso env ?bindings types env2)
           (argso ?bindings args)
           (assoco env2 ?name ['loop args types type] env3)
           (last-typeo env3 ?exprs type env4)
           (assoco env4 form type new-env))))

(defnu recuro ; (recur f args*)
  [env form type new-env]
  ([_ [_ ?f . ?args] _ _]
    (fresh [args types rtype env2]
           (geto ?f ['loop args types rtype] env)           
           (typeso env ?args types env2)
           (== type 'void)
           (assoco env2 form type new-env))))

(defnu leto ; (let* bindings body*)
  [env form type new-env]
  ([_ [_ ?bindings . ?exprs] _ _] ; normal let
    (fresh [types env2 env3]
           (bindingso env ?bindings types env2)
           (last-typeo env2 ?exprs type env3)
           (assoco env3 form type new-env))))

(defnu applyo ; (f args*)
  [env form type new-env]
  ([_ [?f . ?args] _ _] 
    (fresh [env2 template op types env3]
           (typedo env ?f template env2) 
           (typeso env2 ?args types env3)
           (geno template [op types type])
           (assoco env3 form type new-env))))

(defnu binopo
  [env form type new-env]
  ([_ [_ ?a1 ?a2] _ _]
  (fresh [env2 env3]
         (typedo env ?a1 type env2)
         (typedo env2 ?a2 type env3)
         (assoco env3 form type new-env))))

(defnu binpredo
  [env form type new-env]
  ([_ [_ ?a1 ?a2] _ _]
  (fresh [t env2 env3]
         (typedo env ?a1 t env2)
         (typedo env2 ?a2 t env3)
         (== type 'boolean)
         (assoco env3 form type new-env))))
    
(defnu dot ; (. obj member args*)
  [env form type new-env]
  ([_ [_ ?obj ?member . ?args] _ _]
    (fresh [env2 clazz members membert argst env3]
           (typedo env ?obj ['pointer clazz] env2)
           (geto clazz ['class clazz members] env)
           (geto ?member membert members)
           (typeso env2 ?args argst env3)
           (matcha [membert type]
                   ([['method argst type] type]) 
                   ([type type]))
           (assoco env3 form type new-env))))
                   
(defnu newo ; (new Class args*)
  [env form type new-env]
  ([_ [_ ?class . ?args] _ _]
    (fresh [argst members env2]
           (typeso env ?args argst env2)
           (geto ?class ['class ?class members] env)
           (geto :new argst members)
           (== type ['pointer ?class])
           (assoco env2 form type new-env))))

(defnu defo  ; (def name expr)
  [env form type new-env]
  ([_ [_ ?name ?expr] _ _]
    (fresh [env2 env3]
           (updateo env ?name type env2)
           (typedo env2 ?expr type env3)
           (assoco env3 form type new-env)))
  ([_ [_ ?name] _ _]
    (fresh [env2]
           (annotatedo env ?name type env2)
           (assoco env2 form type new-env))))

(defnu doo ; (do exprs*)
  [env form type new-env]
  ([_ [_ . ?exprs] _ _]
    (fresh [env2]
           (last-typeo env ?exprs type env2)
           (assoco env2 form type new-env))))

(defn include* 
  [path]
  (if (= path "iostream") 
    {}
    (includes/include path)))
    
(defn includeo
  [env form type new-env]
  (fn [a]
    (let [genv (walk a env)
          gform (walk a form)
          include (second gform)]
      (if (genv gform)
        (unify a [type new-env] ['void genv])
        (unify a [type new-env] ['void (combine (assoc genv gform ['void]) (include* include))])))))

(defn arrayo
  [env form type new-env]
  (fn [a]
    (let [genv (walk a env)
          gform (walk a form)
          gtype (list 'pointer (second gform))]
      (unify a [type new-env] [gtype (update-in genv [gform] conj gtype)]))))
      
(defn structo 
  [env form type new-env]
  (fn [a]
    (let [genv (walk a env)
          [_ name & members :as gform] (walk a form)
          gtype (list 'struct name (to-env members))]
      (unify a [type new-env] [gtype (update-in genv [gform] conj gtype)]))))
     
(def ^:private expandables 
  (map #(symbol (str "_" %)) (range 0 10)))

(defn expand-type
  [type]
  (cond (coll? type) (walk/postwalk-replace 
                       (zipmap expandables (repeatedly lvar))
                       type)
        :else type))

(defn geno
  [template fresh]
  (fn [a]    
    (let [gtemplate (walk a template)] 
      (unify a fresh (expand-type gtemplate)))))

; OPTIMIZE
(defn annotatedo 
  ([env form type new-env]
    (fresh []
      (annotatedo form type)
      (updateo env form type new-env)))
  ([form type]
    (fn [a]
      (let [gf (walk a form)]
        (if-let [t (-> gf meta :tag)]
          (unify a type (expand-type t)))))))

(defn literalo
  [env form type new-env]
  (fn [a]
    (let [gform (walk a form)]
      (if-let [t (literal-types (.getClass gform))]
        (unify a [type new-env] [t (assoc (walk a env) gform (vector t))])))))

(defn failo
  [form]
  (fn [a]
    (throw (IllegalStateException. 
             (str "Type inference failed for " (walk a form))))))

; OPTIMIZE
(defnu last-typeo
  [env args last new-env]
  ([_ [?a] _ _]
    (typedo env ?a last new-env))    
  ([_ [?a . ?rest] _ _]
    (fresh [t env2]
           (typedo env ?a t env2)
           (last-typeo env2 ?rest last new-env))))

; OPTIMIZE
(defnu typeso
  [env args types new-env]
  ([_ [?a . ?r] [?at . ?rt] _]
    (fresh [env2]
           (typedo env ?a ?at env2)          
           (typeso env2 ?r ?rt new-env)))
  ([?e [] [] ?e]))

(def skipped #{'ns* 'comment})

(def handlers
  {'if ifo 'fn* fno 'let* leto 'loop* loopo 'recur* recuro
   '. dot 'new newo 'def defo 'do doo 'include includeo
   'array arrayo 'struct structo
   '= binpredo '!= binpredo '< binpredo '> binpredo '<= binpredo '>= binpredo 
   '+ binopo '- binopo '* binopo '/ binopo '% binopo})

(defn seqo
  [env form type new-env]
  (fn [a]
    (let [name (first (walk a form))]
      (if (skipped name)
        (unify a env new-env)
        (let [f (or (handlers name) applyo)]        
          ((f env form type new-env) a))))))
 
; TODO special handling of sets, maps and vectors
(defna typedo
  [env form type new-env]
  ([?e nil _ ?e] (== type 'void))
  ([_ [_ . _] _ _] (seqo env form type new-env))
  ([_ _ _ _] (conda ((geto form type env)
                    (== env new-env))
                  ((annotatedo env form type new-env))
                  ((literalo env form type new-env))
                  ((failo form)))))

(defn new-env 
  [env form]
  (first 
    (run 1 [q]
         (fresh [type] (typedo env form type q)))))

(defn type-and-env
  [env form]
  (first
    (run 1 [type env2]
         (typedo env form type env2))))

(defn typeof
  ([form]
    (typeof {} form))
  ([env form]
    (first
      (run 1 [type] 
           (fresh [env2]
                  (typedo env form type env2))))))