(ns symbol.logic-test
  (:refer-clojure :exclude [== reify inc type])
  (:require [clojure.walk :as walk])
  (:use clojure.core.logic))

(defn printo
  [form]
  (fn [a]
    (println (walk a form))
    (unify a [form] [form])))

(defnu testo
  [arg]
  (['f] (printo arg))
  ([['f . _]] (printo arg))  
  ([_] (printo arg)))