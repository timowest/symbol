;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.common
  (:require [clojure.walk :as walk]))

(defn form?
  [form s]
  (and (seq? form) (= (first form) s)))

(defn complex? 
  [form]
  (and (seq? form)
       ('#{if let* loop* do} (first form))))

(def literal-types
  {Long      'long
   Double    'double
   String    'string
   Character 'char
   Boolean   'boolean
   clojure.lang.Ratio 'ratio
   clojure.lang.Symbol 'symbol})

(def operators #{'= '!= '+ '- '* '/ '% '> '< '>= '<= '& '| '<< '>>})

(def core-env   
  '{nil   [void]
    
     ; special forms 
    set!  [(sf [_0 _0] void)] 
    pset! [(sf [(pointer _0) _0] void)
           (sf [(pointer _0) long _0] void)]
    pref  [(sf [(pointer _0)] _0)
           (sf [(pointer _0) long] _0)]
    not   [(sf [boolean] boolean)]
    delete [(sf [(pointer _0)] void)]
    
    ; operators
    =     [(fn [_0 _0] boolean)] 
    !=    [(fn [_0 _0] boolean)]
    <     [(fn [_0 _0] boolean)]
    >     [(fn [_0 _0] boolean)]
    <=    [(fn [_0 _0] boolean)]
    >=    [(fn [_0 _0] boolean)]
    +     [(fn [_0 _0] _0)]
    -     [(fn [_0 _0] _0)]
    *     [(fn [_0 _0] _0)]    
    /     [(fn [_0 _0] _0)]
    %     [(fn [_0 _0] _0)]
    
    ; XXX maybe these should be macros instead ?
    ; casts 
    short [(fn [_0] short)]
    ushort [(fn [_0] ushort)]
    int [(fn [_0] int)]
    uint [(fn [_0] uint)]
    long [(fn [_0] long)]
    ulong [(fn [_0] ulong)]
    float [(fn [_0] float)]
    ufloat [(fn [_0] ufloat)]
    double [(fn [_0] double)]
    udouble [(fn [_0] udouble)]
    ldouble [(fn [_0] ldouble)]
    uldouble [(fn [_0] uldouble)]
        
    ; IO operators (temporary)
    std/cout [out]
    <<    [(fn [_0 _1] _0)]
    std/cin [in]
    >>    [(fn [_0 _1] _0)]})

(defn to-env 
  [coll]
  (reduce
    (fn [acc [k v]] (update-in acc [k] conj v))
    {}
    coll))

