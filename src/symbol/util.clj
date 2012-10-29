(ns symbol.util
  (:require [clojure.walk :as walk]))

(defn form?
  [form s]
  (and (seq? form) (= (first form) s)))

(defn complex? 
  [form]
  (and (seq? form)
       ('#{if let* loop* do} (first form))))

(def  literal-types
  {Long      'long
   Double    'double
   String    'string
   Character 'char
   Boolean   'boolean
   clojure.lang.Ratio 'ratio
   clojure.lang.Symbol 'symbol})
