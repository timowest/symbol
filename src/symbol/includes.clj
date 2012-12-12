;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.includes
  (:import [java.io File])
  (:use [clojure.data.zip.xml :only (attr attr= text xml-> xml1->)] 
        [clojure.java.shell :only [sh]])
  (:require [clojure.data.zip :as zf]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [clojure.zip :as zip]))

; TODO make this independent of the gcc version
(def default-paths
  [""
   "/usr/local/include"
   "/usr/include/c++/4.6"
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
  '{"char" char
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
                       {k key :cat type})]))))

(defn to-arg
  [arg]
  {:name (symbol (xml1-> arg (attr :name)))
   :type (xml1-> arg (attr :type))})                

(defn with-args
  [xml content]
  (into {} (for [[id entry] content]
             (let [args (xml-> xml (attr= :id id) :Argument)]
               [id 
                (assoc entry :args (map to-arg args))]))))

(declare shortdef fulldef)

(defn argtypes
  [all args]
  (map #(shortdef all (:type %)) args)) 

(defn convert-name
  [name]
  (-> name    
    (str/replace #"," " ") ; replaces commas
    (str/replace #"::" "/") ; replace delmiters
    (str/replace #"([\w/]+)<([\w/]+)" "($1 $2") ; replace <
    (str/replace #"\s*>" ")") ; replace >
    (read-string)))

(def shortdefs
  {:ArrayType (fn [all t] (list 'array (shortdef all (:type t)) (:size t)))
   :CvQualifiedType (fn [all t] (shortdef all (:type t)))
   :FundamentalType (fn [all t] (cpp-types (:name t)))
   :PointerType (fn [all t] (list 'pointer (shortdef all (:type t))))
   ;:ReferenceType (fn [all t] (list 'reference (shortdef all (:type t))))
   :ReferenceType (fn [all t] (shortdef all (:type t)))
   :Typedef (fn [all t] (shortdef all (:type t)))      
   :FunctionType (fn [all t] (list 'fn
                                   (argtypes all (:args t))
                                   (shortdef all (:returns t))))            
   :Enumeration (fn [all t] 'int)
   :EnumValue (fn [all t] 'int)
   :Union (fn [all t] (symbol (:id t)))
   :Class (fn [all t] (convert-name (:name t)))
   :Struct (fn [all t] (convert-name (or (:name t) (:id t))))})
    
(def fulldefs 
  (merge 
    shortdefs
    {:Constructor (fn [all t] (list :new (argtypes all (:args t))))
     :Destructor (fn [all t] (list))
     :Method (fn [all t] (list (symbol (:name t)) 
                               (list 'method
                                     (argtypes all (:args t))
                                     (shortdef all (:returns t)))))
     :OperatorMethod (fn [all t] (list (symbol (:name t)) 
                                       (list 'op 
                                             (argtypes all (:args t))
                                             (shortdef all (:returns t)))))
     :Field (fn [all t] (list (symbol (:name t)) (shortdef all (:type t))))
     :Variable (fn [all t] (list (symbol (:name t)) (shortdef all (:type t))))
     :Converter (fn [all t] (list 'converter (symbol (:id t)))) ; TODO
     :Union (fn [all t] (list 'struct (symbol (:id t))
                            (map #(fulldef all %)
                                 (.split (:members t) " "))))
     :Typedef (fn [all t] (list 'typedef (symbol (:name t))
                                (shortdef all (:type t))))
     :Class (fn [all t] (let [name (convert-name (:name t))]
                          (if (:members t) 
                            (let [members (map #(fulldef all %) 
                                               (.split (:members t) " "))]
                              (list 'class name members))
                            (list 'class name))))     
     :Struct (fn [all t] (let [name (convert-name (or (:name t) (:id t)))]
                           (if (:members t)
                             (list 'struct name
                                   (map #(fulldef all %) 
                                        (filter #(pos? (.length %)) (.split (:members t) " "))))
                             (list 'struct name))))}))
             
(defn typedef
  ([types functions id]
    (let [type (types id)
          f (functions (:cat type))]
      (if f 
        (f types type)
        (throw (IllegalStateException. (str "No function for '" id "'")))))))

(defn shortdef
  [types id]
  (typedef types shortdefs id))

(defn fulldef
  [types id]
  (typedef types fulldefs id))

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
    (with-args xml                  
      (xml-get xml :FunctionType :id :returns))
    (xml-get xml :Class :id :name :members)
    (xml-get xml :Struct :id :name :members)))

(defn get-members
  [xml]
  (merge 
    (with-args xml
      (xml-get xml :Constructor :id :name))
    (xml-get xml :Destructor :id)
    (xml-get xml :Converter :id :name :returns)
    (xml-get xml :Variable :id :name :type)
    (with-args xml
      (xml-get xml :Method :id :name :returns))
    (with-args xml
      (xml-get xml :OperatorMethod :id :name :returns))
    (xml-get xml :Field :id :name :type)))

(defn include
  [local-path]
  (if-let [f (get-file default-paths local-path)]
    (let [temp (doto (File/createTempFile "gccxml" "xml")
                 (.deleteOnExit))
          out  (sh "gccxml" (.getAbsolutePath f) (str "-fxml=" (.getAbsolutePath temp)))
          xml  (get-xml temp)          
          types (get-types xml)
          contents (merge types (get-members xml))
          shortdefs (into {} (map (fn [id] [id (shortdef types id)]) 
                                 (keys types)))
          fulldefs (into {} (map (fn [id] [id (fulldef contents id)]) 
                                 (keys contents)))               
          complex    (for [[_ v] fulldefs 
                           :when (and (seq? v) (#{'class 'struct 'typedef} (first v)))] 
                       (list (second v) v))
          enumerations (for [enum (xml-get xml :Enumeration :id :name)]
                         (list (:name enum) 'int))
          enumvalues (for [enumvalue (xml-get xml :EnumValue :id name)]
                       (list (symbol (:name enumvalue)) 'int))
          ; TODO only top level vars
          variables (for [[id v] (xml-get xml :Variable :id :name :type)]
                      (list (symbol (:name v)) (shortdefs (:type v))))            
          functions (for [function (xml-> xml :Function)]
                      (list (symbol (xml1-> function (attr :name)))
                            (list 'fn
                                  (map shortdefs (xml-> function :Argument (attr :type)))
                                  (shortdefs (xml1-> function (attr :returns))))))]
      (concat complex enumerations enumvalues variables functions))))

;(def include (memoize include*))

(defn include-pp
  [local-path]
  (doseq [entry (include local-path)]
    (println entry)))
          
      