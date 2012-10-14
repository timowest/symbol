(ns symbol.types
  (:refer-clojure :exclude [== reify inc type])
  (:use [clojure.core.logic]))

(declare typedo typeso annotatedo)

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
  [env form type]
  ([_ ['if ?c ?t ?e] _] 
    (typedo env ?t type) 
    (typedo env ?e type))
  ([_ ['if ?c ?t] _] (typedo env ?t type)))

(defne fno ; (fn args body)
  [env form type]
  ([_ ['fn [?arg . ?rest] . ?body] ['fn [?argt . ?argst] ?type]]
    (fresh [new-env new-fn]
           (conda 
             ((annotatedo ?arg ?argt))
             ((== ?argt ?argt)))             
           (conso [?arg ?argt] env new-env)
           (appendo ['fn ?rest] ?body new-fn)
           (fno new-env new-fn ['fn ?argst ?type])))
  ([_ ['fn [] ?stmt . _] ['fn [] ?type]] ; TODO should be last stmt
    (typedo env ?stmt ?type)))
                              
(defne bindingso
  [env bindings kt t]
  ([_ [?k ?v . ?rest] [[?k ?vt] . ?rest-kt] [?vt . ?rest-t]]
    (fresh [new-env]
           (conda
             ; TODO type of v should still be inferred and made available to outside
             ((annotatedo ?k ?vt)) 
             ((typedo env ?v ?vt)))
           (conso [?k ?vt] env new-env)
           (bindingso new-env ?rest ?rest-kt ?rest-t)))
  ([_ [] [] []]))    

(defne leto ; (let* bindings body*)
  [env form type]
 ([_ ['let* ?name ?bindings ?stmt . _] _] ; named let
    (fresh [kt types new-env new-env2]
           (bindingso env ?bindings kt types)
           (appendo kt env new-env)
           (conso [?name ['fn types type]] new-env new-env2)
           (typedo new-env2 ?stmt type)))  
  ([_ ['let* ?bindings ?stmt . _] _] ; normal let
    (fresh [kt types new-env]
           (bindingso env ?bindings kt types)
           (appendo kt env new-env)
           (typedo new-env ?stmt type))))

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

(defne defo ; (def name expr)
  [env form type]
  ([_ ['def ?name ?expr] _]
    (fresh [new-env]
           (conso [?name type] env new-env)
           (typedo new-env ?expr type))))
            
(def ^:private literal-types
  {Long      'long
   Double    'double
   String    'string
   Character 'char
   Boolean   'boolean
   clojure.lang.Ratio     'ratio})
    
(defn annotatedo
  [form type]
  (fn [a]
    (let [gf (walk a form)]
      (if-let [t (-> gf meta :tag)]
        (unify a [type] [t])))))

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
(defnu typedo
  [env form type]
  ([_ ['if . _] _] (ifo env form type))
  ([_ ['fn . _] _] (fno env form type))
  ([_ ['let* . _] _] (leto env form type))
  ([_ [?dot . _] _] (== ?dot '.) (dot env form type))
  ([_ ['new . _] _] (newo env form type))
  ([_ ['def . _] _] (defo env form type))
  ([_ [?fn . _] _] (applyo env form type))    
  ([_ _ _] (conda ((membero [form type] env))
                  ((annotatedo form type))
                  ((literalo form type))))) 
