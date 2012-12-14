;   Copyright (c) Timo Westkämper. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns symbol.integration
  (:import [java.io File])
  (:use [clojure.java.shell :only [sh]] 
        midje.sweet)
  (:require [symbol.compiler :as compiler]))

(defn dump
  [file]
  (let [str (compiler/read-emit file)
        temp (doto (File/createTempFile "symbol" ".cpp")
               (.deleteOnExit))]
    (println str)
    (spit temp str)
    (sh "g++" "-shared" "-std=c++0x" (.getAbsolutePath temp))))
    
(def ok {:exit 0 :out "" :err ""})

(facts "simple"
  (dump "dev-resources/tests/simple.s") => ok
  (dump "dev-resources/tests/osc.s") => ok)
  ; TODO osc.s
  ; TODO io/example*
        
  
