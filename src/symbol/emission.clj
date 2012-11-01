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
  (:use symbol.util))

; if fn* let* loop* recur* . new def do 
; set! pset! pref 
; < > <= >= + - * /

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

(defmethod emit 'if 
  [env target form]
  (let [[_ c t e] form  
        ce (emit env nil c)
        te (emit env target t)
        ee (if e (emit env target e))]
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
               
(defn stmt 
  [& args]
  (let [s (string/join " " args)] 
    (if (.endsWith s "}") s (str s ";"))))

(defn lines 
  [& l]
  (string/join "\n" (keep identity (flatten l))))

(defn stmts
  [env target body]
  (when (seq body)
    (let [start (map #(emit env nil %) (butlast body))
          end (emit env target (last body))]
      (lines (map stmt (concat start [end]))))))  

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
        args-str (args->string env args argtypes)]
    (lines 
      (str "[](" args-str "){")
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
  (let [[_ name & args] form]
    ; TODO bindings
    (str "goto " name)))

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

(defmethod emit 'def
  [env target form]
  (let [[_ name value] form
        type (get-type env name)]
    (if (form? value 'fn*)
      (let [[_ arg-types rtype] type
            rtypes (type->string env rtype)
            [args & body] (second value)
            args-str (args->string env args arg-types)]
        (lines
          (fn-generics type)
          (format "%s %s(%s) {" rtypes name args-str)
          (fn-body env target body rtype)
          "}"))        
      (stmt (type->string env type) name "=" (emit env nil value))))) 

(defmethod emit 'do
  [env target form]
  (stmts env target (rest form)))

(defmethod emit 'long
  [env target form]
  (str form))

(defmethod emit 'string
  [env target form]
  (str "\"" form "\""))

(defmethod emit 'char
  [env target form]
  (str "'" form "'"))

(defmethod emit 'boolean
  [env target form]
  (str form))

(defmethod emit 'ratio
  [env target form]
  (str (double form)))

(defmethod emit 'symbol
  [env target form]
  (str form))

(def math-ops 
  (into {} (for [k '#{+ - * / < > <= >= != ==}]
             [k (str " " k " ")])))

(def unary-ops '{not "!" + "+" - "-"}) 

(defmethod emit :default
  [env target form]
  (if (seq? form)
    (let [f (first form)
          r (map #(emit env nil %) (rest form))
          unary (= 1 (count r))
          val (cond (and unary (unary-ops f)) (str (unary-ops f) (first r))             
                    (math-ops f) (str "(" (string/join (math-ops f) r) ")")
                    :else (str f "(" (string/join ", " r) ")"))]
      (if target
        (stmt target "=" val)
        val))
    (if target (stmt target "=" form) form)))
  
