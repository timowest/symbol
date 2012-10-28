(ns symbol.emission
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]))

; if fn* let* loop* recur* . new def do 
; set! pset! pref 
; < > <= >= + - * /

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

; TODO move to util
(defn form?
  [form s]
  (and (seq? form) (= (first form) s)))

(defn get-type
  [env form]
  (let [matches (keep (fn [[f t]] (if (= f form) t)) env)]
    (first matches)))

(defn- emit-selector
  [env target form]
  (if (list? form) 
    (first form)
    form))

(defmulti emit emit-selector)

(defmethod emit 'if 
  [env target form]
  (let [[_ c t e] form  
        ce (emit env nil c)
        te (emit env target t)
        ee (if e (emit env target e))]
    (if ee
      (format "if (%s) {\n%s\n} else {\n%s\n}" ce te ee)
      (format "if (%s) {\n%s\n}" ce te))))     

(defn type->string
  [env type]  
  (cond (symbol? type) (cpp-types type)
        ; TODO
        :else nil))

(defn args->string
  [env args types]
  (->> (for [[arg type] (zipmap args types)]
         (str (type->string env type) " " arg))
    (string/join ", ")))
               
(defn stmt 
  [& args]
  (let [combined (string/join " " args)]
    (if (.endsWith combined "}")
      (str combined "\n")
      (str combined ";\n"))))

(defn stmts
  [env target body]
  (let [start (map #(emit env nil %) (butlast body))
        end (emit env target (last body))]
    (str (string/join (map stmt start)) 
         (stmt end))))

(defmethod emit 'fn* ; (fn* (args body)
  [env target form]
  (let [[_ argtypes rtype] (get-type env form)
        [args & body] (second form)
        args-str (args->string env args argtypes)
        return (if (= rtype 'void) nil (gensym))]
    (if return 
      (str "[](" args-str "){\n"
           (stmt (type->string rtype) return)
           (stmts env return body)
           (stmt "return" return) "}")
      (str "[](" args-str "){\n"
           (stmts env nil body) "}"))))
      
(defmethod emit 'let*
  [env target form])

(defmethod emit 'loop*
  [env target form])

(defmethod emit 'recur*
  [env target form])

(defmethod emit '.
  [env target form])

(defmethod emit 'new
  [env target form])

(defmethod emit 'def
  [env target form])

(defmethod emit 'do
  [env target form])

(defmethod emit :default
  [env target form])