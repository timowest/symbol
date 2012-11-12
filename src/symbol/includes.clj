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
            [clojure.xml :as xml]
            [clojure.zip :as zip]))

; TODO make this independent of the gcc version
(def default-paths
  ["/usr/local/include"
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
    "unsigned char" uchar
    "short int" short
    "unsigned short int" ushort
    "short" short
    "unsigned short" ushort
    "int" int
    "unsigned int" uint
    "long int" long
    "unsigned long int" ulong
    "long" long
    "unsigned long" ulong
    "bool" boolean
    "float" float
    "unsigned float" ufloat
    "double" double
    "unsigned double" double
    "long double" ldouble
    "unsigned long double" uldouble})
          
; TODO cache includes, maybe via memoize

(defn xml-get
  [xml type k v]
  (into {} (for [entry (xml-> xml type)]
    [(xml1-> entry (attr k))
     (xml1-> entry (attr v))])))

(defn mapv
  [f m]
  (into {} (for [[k v] m] [k (f v)])))
             
; TODO ReferenceType, ArrayType, Struct
; TODO normalize type names
(defn include
  [local-path]
  (if-let [f (get-file default-paths local-path)]
    (let [temp (doto (File/createTempFile "gccxml" "xml")
                 (.deleteOnExit))
          out  (sh "gccxml" (.getAbsolutePath f) (str "-fxml=" (.getAbsolutePath temp)))
          xml  (get-xml temp)
          ; TODO improve type handling, maybe via reduce or loop
          fundamentals (->> (xml-get xml :FundamentalType :id :name)
                         (mapv cpp-types))
          cvqualified (->> (xml-get xml :CvQualifiedType :id :type)
                        (mapv fundamentals))                    
          types (merge fundamentals cvqualified)
          pointers (->> (xml-get xml :PointerType :id :type)
                    (mapv #(list 'pointer (types %))))          
          types (merge types pointers)
          functions (for [function (xml-> xml :Function)]
                      (list (xml1-> function (attr :name))
                            (list 'fn
                                  (map types (xml-> function :Argument (attr :type)))
                                  (types (xml1-> function (attr :returns))))))]
      (remove #(.startsWith (first %) "__") functions))))

(defn include-pp
  [local-path]
  (doseq [function (include local-path)]
    (println function)))
          
      