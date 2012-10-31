;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

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
