;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.emission  
  (:refer-clojure :exclude [munge])
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint])
  (:use symbol.common))

(declare type->string)

(def ^:private cpp-types
  '{void    "void"
    string  "std::string"
    boolean "bool"
    char    "char"
    short   "short"
    int     "int"
    long    "long"
    float   "float"
    double  "double"
    uchar   "unsigned char"
    ushort  "unsigned short"
    uint    "unsigned int"
    ulong   "unsigned long"
    ufloat  "unsigned float"
    udouble "unsigned double"})

(def ^:private char-map
  {\- "_",
   \: "_COLON_",
   \+ "_PLUS_",
   \> "_GT_",
   \< "_LT_",
   \= "_EQ_",
   \~ "_TILDE_",
   \! "_BANG_",
   \@ "_CIRCA_",
   \# "_SHARP_",
   \' "_SINGLEQUOTE_",
   (char 34) "_DOUBLEQUOTE_", 
   \% "_PERCENT_",
   \^ "_CARET_",
   \& "_AMPERSAND_",
   \* "_STAR_",
   \| "_BAR_",
   \{ "_LBRACE_",
   \} "_RBRACE_",
   \[ "_LBRACK_",
   \] "_RBRACK_",
   ;\/ "_SLASH_",
   \/ "::"
   \\ "_BSLASH_",
   \? "_QMARK_",
   \. "_DOT_"})

(defn munge [s]
  (apply str (map #(char-map % %) (str s))))

(def generics 
  (zipmap (map #(symbol (str "_" %)) (range 0 26))
          (map str "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(defn get-type
  [env form]
  (or 
    (first (env form))
    (throw (IllegalStateException. (str "Found no type for " form)))))  

(defn- emit-selector
  [env target form]
  (cond (seq? form) (first form)
        (literal-types (type form)) (literal-types (type form))
        :else form))

(defmulti emit emit-selector)

(defn lines 
  [& l]
  (string/join "\n" (keep identity (flatten l))))

(defn stmts
  [env target body]
  (when (seq body)
    (let [start (map #(emit env :stmt %) (butlast body))
          end (emit env (or target :stmt) (last body))]
      (lines (concat start [end])))))  

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
        (symbol? t) (emit env nil t)
        (form? (seq t) 'fn) (fn-type->string env t)
        (form? (seq t) 'pointer) (str (type->string env (second t)) "*")
        :else (-> t type str)))

(defn args->string
  [env args types]
  (string/join
    ", "
    (for [[arg type] (zipmap args types)]
      (str (type->string env type) " " arg))))

(defn fn-body
  [env target body rtype]
  (let [l (last body)
        return (when-not (= rtype 'void) (gensym))]
    (if return
      (lines
        (stmts env nil (butlast body))
        (if (complex? l)
          (lines
            (str (type->string env rtype) " " return ";")
            (emit env return l)
            (str "return " return ";"))
          (str "return " (emit env nil l) ";")))
      (stmts env nil body))))  

(defn assignment
  [env [name value]]
  (let [type (type->string env (get-type env name))
        const (:const (meta name))]
    (if (complex? value)
      (lines
        (str type " " name ";")
        (emit env name value))
      (if const
        (str "const " type " " name " = " (emit env nil value) ";")
        (str type " " name " = " (emit env nil value) ";")))))    

(def math-ops 
  (let [base (into {} (for [k '#{+ - * / < > <= >= != << >> %}]
                        [k (str " " k " ")]))]
    (merge base '{= " == "})))

(def unary-ops '{not "!" + "+" - "-"}) 

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
        args-str (args->string env args arg-types)
        name-str (emit env nil name)]
    (lines
      (fn-generics type)
      (format "%s %s(%s) {" rtypes name-str args-str)
      (fn-body env nil body rtype)
      "}\n")))

(defn def-struct
  [env name value]
  (let [[_ name members] (get-type env name)]
    (lines
      (str "struct " (emit env nil name) " {")
      (for [[name types] members :when (not (= name :new))
            type types]        
        (str (type->string env type) " " (emit env nil name) ";"))
      "};\n")))

; defmethods in alphabetic order

(defmethod emit 'array
  [env target form]
  (let [[_ type dimensions] form]
    (str "new " (type->string env type) "[" dimensions "]")))

(defmethod emit 'cast
  [env target form]
  (let [[_ type expr] form]
    (str "(" (type->string env type) ")" "(" (emit env nil expr) ")")))

(defmethod emit 'comment
  [env target form]
  "\n")

(defmethod emit 'def
  [env target form]
  (let [[_ name value] form]
    (cond (form? value 'struct) (def-struct env name value) 
          (form? value 'fn*) (def-fn env name value) 
          :else (assignment env [(with-meta name {:const true}) value]))))

(defmethod emit 'delete
  [env target form]
  (let [[_ value] form]
    (str "delete " (emit env nil value) ";\n")))

(defmethod emit 'do
  [env target form]
  (stmts env target (rest form)))

(defmethod emit 'fn* ; (fn* (args body)
  [env target form]
  (let [[_ argtypes rtype] (get-type env form)
        [args & body] (second form)
        args-str (args->string env args argtypes)
        to-target (if target (str (emit env nil target) " = ") "")]
    (lines 
      (str to-target "[&](" args-str ") {")
      (fn-body env target body rtype)
      (if target "};" "}"))))

(defmethod emit 'if 
  [env target form]
  (let [[_ c t e] form  
        target (or target :stmt)
        ce (emit env nil c)
        te (emit env target t)
        ee (if e (emit env target e))]
    (cond (form? e 'if) (format "if (%s) {\n%s\n} else %s" ce te ee)
          ee           (format "if (%s) {\n%s\n} else {\n%s\n}" ce te ee)
          :else        (format "if (%s) {\n%s\n}" ce te))))     

(defmethod emit 'include
  [env target form]
  (str "#include \"" (second form) "\"\n"))

(defmethod emit 'let*
  [env target form]
  (let [[_ bindings & body] form
        bind-pairs (partition 2 bindings)]                      
    (lines 
      (loop [acc [] pairs bind-pairs seen #{}]
        (if (seq pairs)
          (let [[name value] (first pairs)
                line (if (seen name)
                       (emit env name value)
                       (assignment env (first pairs)))]
            (recur (conj acc line) (rest pairs) (conj seen name)))
          acc))
      (stmts env target body))))
        
(defmethod emit 'loop* 
  [env target form]
  (let [[_ name bindings & body] form
        bind-pairs (partition 2 bindings)]
    (lines
      (map #(assignment env %) bind-pairs)
      (str name ":") 
      (stmts env target body))))

(defmethod emit 'new  
  [env target form]
  (let [[_ clazz & args] form
        args (map #(emit env nil %) args)]
    (format "new %s(%s)" clazz (string/join ", " args))))

(defmethod emit 'ns*
  [env target form]
  (let [[_ name] form]
    (str "//ns " name)))

(defmethod emit 'pref
  [env target form]
  (let [pointer (emit env nil (second form))
        idx (emit env nil (last form))
        s (format "%s[%s]" pointer idx)]
  (cond (= target :stmt) (str s ";")
          (nil? target) s
          :else (str target " = " s ";"))))

(defmethod emit 'pset!
  [env target form]
  (let [pointer (emit env nil (second form))
        value (emit env nil (last form))
        idx (when (> (count form) 2)
              (emit env nil (nth form 2)))]
    (if idx
      (format "%s[%s] = %s;" pointer idx value)
      (format "*%s = %s;" pointer value))))

(defmethod emit 'recur* 
  [env target form]
  (let [[_ name & args] form
        [_ names types rtype] (get-type env name)
        bind-pairs (zipmap names args)]
    (lines 
      (for [[k v] bind-pairs] (emit env k v))
      (str "goto " name ";"))))

(defmethod emit 'set!
  [env target form]
  (let [[_ to expr] form]
    (emit env to expr)))

(defmethod emit 'use
  [env target form]
  (let [path (.replace (str (second form)) "." "/")]
    (str "#include \"" path ".cpp" "\"\n")))

(defmethod emit '. ; TODO
  [env target form]
  (let [[_ obj member & args] (map #(emit env nil %) form)]
    (if (empty? args)
      (str obj "->" member)
      (str obj "->" member "(" (string/join ", " args) ")"))))

(defn emit-signature
  [env [f & r]]
  (let [[_ gent rt] (get-type env f)
        argst (map #(get-type env %) r)]
    (if-not (or (< (count gent) 2) (= gent argst))
      (str "<" (string/join ", " (map second (sort-by first (zipmap gent argst)))) ">")
      "")))

(defn emit-apply
  [env form]
  (let [f (first form)
        r (map #(emit env nil %) (rest form))
        unary (= 1 (count r))]
    (cond (and unary (unary-ops f)) (str (unary-ops f) (first r))             
          (math-ops f) (str "(" (string/join (math-ops f) r) ")")
          (cpp-types f) (str "(" (cpp-types f) ")" (first r))
          :else (str (emit env nil f) 
                     (emit-signature env form) 
                     "(" (string/join ", " r) ")"))))

(defmethod emit :default
  [env target form]
  (let [s (cond (seq? form) (emit-apply env form)
                (symbol? form) (munge (str form))
                ; TODO proper string escaping
                (string? form) (str "\"" (string/escape form {\newline "\\n" }) "\"")
                (char? form) (str "'" form "'")
                :else (str form))]
    (cond (= target :stmt) (str s ";")
          (nil? target) s
          :else (str (emit env nil target) " = " s ";"))))

; formatted output

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
  
