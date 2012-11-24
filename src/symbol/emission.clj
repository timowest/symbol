;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.emission
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint])
  (:use symbol.common))

(declare type->string)

(def cpp-types
  '{string  string
    boolean boolean
    char   int8_t
    short  int16_t
    int    int32_t
    long   int64_t
    uchar  uint8_t
    ushort uint16_t
    uint   uint32_t
    ulong  uint64_t})

(def generics 
  (zipmap (map #(symbol (str "_." %)) (range 0 26))
          (map str "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(defn get-type
  [env form]
  (let [matches (for [[f t] env :when (= f form)] t)]
    (first matches)))

(defn- emit-selector
  [env target form]
  (cond (seq? form) (first form)
        (literal-types (type form)) (literal-types (type form))
        :else form))

(defmulti emit emit-selector)

(defn stmt 
  [& args]
  (let [s (string/join " " args)] 
    (cond (.endsWith s "}") s
          (.endsWith s ";") s
          :else (str s ";"))))

(defn lines 
  [& l]
  (string/join "\n" (keep identity (flatten l))))

(defn stmts
  [env target body]
  (when (seq body)
    (let [start (map #(emit env nil %) (butlast body))
          end (emit env target (last body))]
      (lines (map stmt (concat start [end]))))))  

(defmethod emit 'if 
  [env target form]
  (let [[_ c t e] form  
        ce (emit env nil c)
        te (stmt (emit env target t))
        ee (if e (stmt (emit env target e)))]
    (cond (form? e 'if) (format "if (%s) {\n%s\n} else %s" ce te ee)
          ee           (format "if (%s) {\n%s\n} else {\n%s\n}" ce te ee)
          :else        (format "if (%s) {\n%s\n}" ce te))))     

(defn fn-type->string
  [env [_ arg-types rtype]]
  (let [t->s #(type->string env %)
        rt (t->s rtype)
        args (string/join ", " (map t->s arg-types))]
    (format "std::function<%s(%s)>" rt args)))            

(defn type->string
  [env t]  
  (cond (cpp-types t) (cpp-types t)
        (generics t) (generics t)
        (symbol? t) (str t)
        (form? (seq t) 'fn) (fn-type->string env t) 
        :else (-> t type str)))

(defn args->string
  [env args types]
  (->> (for [[arg type] (zipmap args types)]
         (str (type->string env type) " " arg))
    (string/join ", ")))

(defn fn-body
  [env target body rtype]
  (let [l (last body)
        return (if (= rtype 'void) nil (gensym))]
    (if return
      (lines
        (stmts env nil (butlast body))
        (if (complex? l)
          (lines
            (stmt (type->string env rtype) return)
            (stmt (emit env return l))
            (stmt "return" return))
          (stmt "return" (emit env nil l))))
      (stmts env nil body))))  

(defmethod emit 'fn* ; (fn* (args body)
  [env target form]
  (let [[_ argtypes rtype] (get-type env form)
        [args & body] (second form)
        args-str (args->string env args argtypes)
        to-target (if target (str (emit env nil target) " = ") "")]
    (lines 
      (str to-target "[](" args-str "){")
      (fn-body env target body rtype)
      "}")))
      
(defn assignment
  [env [name value]]
  (let [type (type->string env (get-type env name))]
    (if (complex? value)
      (lines
        (stmt type name)
        (stmt (emit env name value)))
      (stmt type name "=" (emit env nil value)))))    

(defmethod emit 'let*
  [env target form]
  (let [[_ bindings & body] form
        bind-pairs (partition 2 bindings)]
    (lines 
      (map #(assignment env %) bind-pairs)
      (stmts env target body))))
        
(defmethod emit 'loop* 
  [env target form]
  (let [[_ name bindings & body] form
        bind-pairs (partition 2 bindings)]
    (lines
      (map #(assignment env %) bind-pairs)
      (str name ":") 
      (stmts env target body))))

(defmethod emit 'recur* 
  [env target form]
  (let [[_ name & args] form
        [_ names types rtype] (get-type env name)
        bind-pairs (zipmap names args)]
    (lines 
      (for [[k v] bind-pairs] (emit env k v))
      (stmt "goto" name))))

(defmethod emit '. ; TODO
  [env target form]
  (string/join " " (map str form)))

(defmethod emit 'new  
  [env target form]
  (let [[_ clazz & args] form
        args (map #(emit env nil %) args)]
    (format "new %s(%s)" clazz (string/join ", " args))))

(defn fn-generics
  [type]
  (if-let [types (->> type flatten distinct (keep generics) seq)]
    (str "template <class " (string/join ", class " types)  ">")))

(defn def-fn
  [env name value]
  (let [type (get-type env name)
        [_ arg-types rtype] type
        rtypes (type->string env rtype)
        [args & body] (second value)
        args-str (args->string env args arg-types)]
    (lines
      (fn-generics type)
      (format "%s %s(%s) {" rtypes name args-str)
      (fn-body env nil body rtype)
      "}")))

(defn def-struct
  [env name value]
  (let [[_ name members] (get-type env name)]
    (lines
      (str "struct " (str name) " {")
      (for [[type name] members]
        (stmt (type->string env type) (str name)))
      "}")))
        
(defmethod emit 'def
  [env target form]
  (let [[_ name value] form]
    (cond (form? value 'struct) (def-struct env name value) 
          (form? value 'fn*) (def-fn env name value) 
          :else (assignment env [name value]))))

(defmethod emit 'do
  [env target form]
  (stmts env target (rest form)))

(defmethod emit 'long
  [env target form]
  (if target
    (str target " = " form)
    (str form)))

(defmethod emit 'string
  [env target form]
  (let [escaped (str "\"" form "\"")]  ; TODO property escaping
    (if target
      (str target " = " escaped)
      escaped))) 

(defmethod emit 'char
  [env target form]
  (let [literal (str "'" form "'")]
    (if target
      (str target " = " literal)
      literal)))

(defmethod emit 'boolean
  [env target form]
  (if target
    (str target " = " form)
    form))

(defmethod emit 'ratio
  [env target form]
  (let [literal (double form)]
    (if target
      (str target " = " literal)
      literal)))

(defmethod emit 'symbol
  [env target form]
  (if target
    (str target " = " form)
    form))

(def math-ops 
  (let [base (into {} (for [k '#{+ - * / < > <= >= !=}]
                        [k (str " " k " ")]))]
    (merge base '{= " == "})))

(def unary-ops '{not "!" + "+" - "-"}) 

(defmethod emit :default
  [env target form]
  (if (seq? form)
    (let [f (first form)
          r (map #(emit env nil %) (rest form))
          unary (= 1 (count r))
          val (cond (and unary (unary-ops f)) (str (unary-ops f) (first r))             
                    (math-ops f) (str "(" (string/join (math-ops f) r) ")")
                    :else (str (emit env nil f) "(" (string/join ", " r) ")"))]
      (if target
        (str target " = " val)
        val))
    (if target 
      (str target " = " form) 
      form)))

(defn- block-in 
  [indent] 
  (str indent "    "))

(defn- block-out 
  [indent] 
  (.substring indent 0 (- (.length indent) 4)))

(defn format-cpp
  [s]
  (loop [acc [] lines (string/split-lines s) indent ""]
    (if (seq lines)
      (let [[f & r] lines
            new-indent (if (.startsWith f "}") (block-out indent) indent)
            out-indent (if (.endsWith f "{") (block-in new-indent) new-indent)]
        (recur (conj acc (str new-indent f)) r out-indent))
      (string/join "\n" acc))))
  
