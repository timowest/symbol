;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.includes
  (:refer-clojure :exclude [short])
  (:import [java.io File])
  (:use [clojure.data.zip.xml :only (attr attr= text xml-> xml1->)] 
        [clojure.java.shell :only [sh]]
        symbol.common)
  (:require [clojure.data.zip :as zf]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [clojure.zip :as zip]))

; TODO make this independent of the gcc version
(def default-paths
  [""
   "includes"
   "/usr/local/include"
   "/usr/include/c++/4.6"
   "/usr/include/c++/4.4"
   "/usr/include"])

(defn get-file
  [search-paths local-path]
  (loop [paths default-paths]
    (let [f (File. (first paths) local-path)]
      (cond (.exists f) f
            (seq paths) (recur (rest paths))))))          

(def get-xml (comp zip/xml-zip xml/parse))

(defn- dump  
  [local-path]
  (if-let [f (get-file default-paths local-path)]
    (slurp f)))

(def cpp-types
  '{"void" void
    "char" char
    "wchar_t" wchar
    "unsigned char" uchar
    "short int" short
    "unsigned short int" ushort
    "short unsigned int" ushort
    "short" short
    "unsigned short" ushort
    "int" int
    "unsigned int" uint
    "long int" long
    "unsigned long int" ulong
    "long unsigned int" ulong
    "long" long
    "unsigned long" ulong
    "bool" boolean
    "float" float
    "unsigned float" ufloat
    "double" double
    "unsigned double" double
    "long double" ldouble
    "unsigned long double" uldouble
    "long unsigned double" uldouble})
          
(defn xml-get
  [xml type k & vs]
  (into {} (for [entry (xml-> xml type)]
             (let [key (xml1-> entry (attr k))]
               [key
                (merge (into {} (for [v vs]
                             [v (xml1-> entry (attr v))]))
                       {k key :cat type :xml entry})]))))

(defn to-arg
  [arg]
  {:name (symbol (or (xml1-> arg (attr :name)) "?")) ; FIXME
   :type (xml1-> arg (attr :type))})                

(defn with-args
  [content]
  (into {} (for [[id entry] content]
             (let [args (xml-> (:xml entry) :Argument)]
               [id 
                (assoc entry :args (map to-arg args))]))))

(declare short full)

(defn argtypes
  [all args]
  (map #(short all (:type %)) args)) 

(defn convert-name
  [name]
  (-> name    
    (str/replace #"," " ") ; replaces commas
    (str/replace #"::" "/") ; replace delmiters
    (str/replace #"([\w/]+)<([\w/]+)" "($1 $2") ; replace <
    (str/replace #"\s*>" ")") ; replace >
    (read-string)))

(defn- selector
  [all t]
  (:cat t))

(defmulti short* selector)

(defmethod short* :ArrayType
  [all t]
  (list 'array (short all (:type t)) (:size t)))

(defmethod short* :CvQualifiedType
  [all t]
  (short all (:type t)))

(defmethod short* :FundamentalType
  [all t]
  (cpp-types (:name t)))

(defmethod short* :PointerType
  [all t]
  (list 'pointer (short all (:type t))))

(defmethod short* :ReferenceType
  [all t]
  (short all (:type t)))

(defmethod short* :Typedef
  [all t]
  (short all (:type t)))

(defmethod short* :FunctionType
  [all t]
  (list 'fn
        (argtypes all (:args t))
        (short all (:returns t))))

(defmethod short* :Enumeration
  [all t]
  'int)

(defmethod short* :EnumValue
  [all t]
  'int)

(defmethod short* :Union
  [all t]
  (symbol (:id t)))

(defmethod short* :Class
  [all t]
  (convert-name (:name t)))

(defmethod short* :Struct
  [all t]
  (convert-name (or (:name t) (:id t))))

(defmethod short* :default
  [all t]
  t)

(defmulti full* selector)

(defmethod full* :Constructor
  [all t]
  (list :new (argtypes all (:args t))))

(defmethod full* :Destructor
  [all t]
  (list :destruct 'void))

(defmethod full* :Method
  [all t]
  (list (symbol (:name t)) 
        (list 'method
              (argtypes all (:args t))
              (short all (:returns t)))))

(defmethod full* :OperatorMethod
  [all t]
  (list (symbol (:name t)) 
        (list 'op 
              (argtypes all (:args t))
              (short all (:returns t)))))

(defmethod full* :Field
  [all t]
  (list (symbol (:name t)) (short all (:type t))))

(defmethod full* :Variable
  [all t]
  (list (symbol (:name t)) (short all (:type t))))

(defmethod full* :Converter
  [all t]
  (list 'converter (symbol (:id t)))) ; TODO

(defmethod full* :Union
  [all t]
  (list 'struct (symbol (:id t))
        (map #(full all %)
             (.split (:members t) " "))))

(comment (defmethod full* :Typedef
  [all t]
  (let [ref (all (:type t))]
    (if (= (:cat ref) :Struct)
      (full* all ref)
      (list 'typedef (symbol (:name t))
            (short all (:type t)))))))

(defmethod full* :Typedef
  [all t]
  (list 'typedef (symbol (:name t))
        (short all (:type t))))

(defmethod full* :Class
  [all t]
  (let [name (convert-name (:name t))]
    (if (:members t) 
      (let [members (map #(full all %) 
                         (.split (:members t) " "))]
        (list 'class name [] (to-env (filter coll? members)))) 
      (list 'class name []))))

(defmethod full* :Struct
  [all t]
  (let [name (convert-name (or (:name t) (:id t)))]
    (if (:members t)
      (let [members (map #(full all %) 
                         (filter #(pos? (.length %)) (.split (:members t) " ")))]
        (list 'struct name [] (to-env (filter coll? members))))                                    
      (list 'struct name []))))

(defmethod full* :default
  [all t]
  (short* all t))

(defn- short
  [all id]
  (short* all (all id)))

(defn- full
  [all id]
  (full* all (all id)))
    
(defn get-types
  [xml]
  (merge 
    (xml-get xml :ArrayType :id :type :size)
    (xml-get xml :CvQualifiedType :id :type)
    (xml-get xml :Enumeration :id :name)
    (xml-get xml :FundamentalType :id :name)                  
    (xml-get xml :PointerType :id :type)
    (xml-get xml :ReferenceType :id :type)
    (xml-get xml :Typedef :id :name :type)
    (xml-get xml :Union :id :members)
    (with-args                   
      (xml-get xml :FunctionType :id :returns))
    (xml-get xml :Class :id :name :members)
    (xml-get xml :Struct :id :name :members)))

(defn get-members
  [xml]
  (merge 
    (with-args 
      (xml-get xml :Constructor :id :name))
    (xml-get xml :Destructor :id)
    (xml-get xml :Converter :id :name :returns)
    (xml-get xml :Variable :id :name :type)
    (with-args 
      (xml-get xml :Method :id :name :returns))
    (with-args 
      (xml-get xml :OperatorMethod :id :name :returns))
    (xml-get xml :Field :id :name :type)))

(defn include*
  [local-path]
  (if-let [f (get-file default-paths local-path)]
    (let [temp (doto (File/createTempFile "gccxml" "xml")
                 (.deleteOnExit))          
          out  (sh "gccxml"
                   (.getAbsolutePath f) 
                   (str "-fxml=" (.getAbsolutePath temp)))
          xml  (get-xml temp)          
          types (get-types xml)
          contents (merge types (get-members xml))
          shorts (into {} (map (fn [id] [id (short types id)]) 
                               (keys types)))
          fulls (into {} (map (fn [id] [id (full contents id)]) 
                              (keys contents)))               
          complex    (for [[_ v] fulls 
                           :when (and (seq? v) (#{'class 'struct 'typedef} (first v)))] 
                       (list (second v) v))
          enumerations (for [enum (xml-get xml :Enumeration :id :name)]
                         (list (:name enum) 'int))
          enumvalues (for [enumvalue (xml-get xml :EnumValue :id :name)]
                       (list (symbol (:name enumvalue)) 'int))
          ; TODO only top level vars
          variables (for [[id v] (xml-get xml :Variable :id :name :type)]
                      (list (symbol (:name v)) (shorts (:type v))))            
          functions (for [function (xml-> xml :Function)]
                      (list (symbol (xml1-> function (attr :name)))
                            (list 'fn
                                  (map shorts (xml-> function :Argument (attr :type)))
                                  (shorts (xml1-> function (attr :returns))))))]
      (->> (concat complex enumerations enumvalues variables functions) 
           (remove #(.startsWith (str (first %)) "__builtin"))
           to-env))
    (throw (IllegalArgumentException. (str "Got no file for " local-path)))))

(def bundled
  (into {} (for [path ["list"]]
             (let [[_ & contents] (read-cp (str path ".s"))]
               [path (to-env (partition 2 contents))]))))

(def include 
  (let [memoized (memoize include*)]
    (fn [path]
      (or (bundled path) (memoized path)))))

(defn- include-pp
  [local-path]
  (doseq [entry (include local-path)]
    (println entry)))
          
      