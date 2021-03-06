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

(def ok {:exit 0 :out "" :err ""})

(defn dump
  [file]
  (let [str (compiler/read-emit file)
        temp (doto (File/createTempFile "symbol" ".cpp")
               (.deleteOnExit))
        _ (spit temp str)
        out (sh "g++" "-shared" "-std=c++0x" (.getAbsolutePath temp))]
    (when-not (= out ok)
      (println str))
    out))   

(facts "simple"
  (dump "dev-resources/tests/simple.s") => ok)

(facts "math"
  (dump "dev-resources/tests/math.s") => ok)

(facts "functions"
  (dump "dev-resources/tests/functions1.s") => ok
  (dump "dev-resources/tests/functions2.s") => ok
  (dump "dev-resources/tests/functions3.s") => ok
  (dump "dev-resources/tests/functions4.s") => ok
  (dump "dev-resources/tests/functions5.s") => ok
  (dump "dev-resources/tests/functions6.s") => ok)

(facts "structs"
  (dump "dev-resources/tests/structs1.s") => ok
  (dump "dev-resources/tests/structs2.s") => ok)

(facts "deftypes"
  (dump "dev-resources/tests/deftypes.s") => ok)

(facts "chars"
  (dump "dev-resources/tests/chars.s") => ok)
       
(facts "pointers"
  (dump "dev-resources/tests/pointers.s") => ok
  (dump "dev-resources/tests/pointers2.s") => ok)

(facts "lists"
  (dump "dev-resources/tests/lists.s") => ok)

(facts "literals"
  (dump "dev-resources/tests/literals.s") => ok)

(facts "use"
  (binding [compiler/*ns-includes* "dev-resources/tests"]
    (compiler/read-emit "dev-resources/tests/uses.s")) => string?)

(facts "audio"
  (dump "dev-resources/tests/audio.s") => ok)

(facts "midi"
  (dump "dev-resources/tests/midi.s") => ok)
        
(facts "io"
  (dump "dev-resources/tests/io/example.s")  => ok
  (dump "dev-resources/tests/io/example2.s") => ok
  (dump "dev-resources/tests/io/example3.s") => ok 
  (dump "dev-resources/tests/io/example4.s") => ok
  (dump "dev-resources/tests/io/example5.s") => ok 
  (dump "dev-resources/tests/io/example6.s") => ok
  (dump "dev-resources/tests/io/example7.s") => ok) 
 
(facts "extern"
  (dump "dev-resources/tests/extern.s") => ok)

(facts "includes"
  (dump "dev-resources/tests/includes.s") => ok)

(comment (facts "gtkmm"
  (dump "dev-resources/tests/gtkmm/simple.s") => ok))

; TEMP
(comment (facts "rogue"
  (dump "../rogue/src/synth.s") => ok
  (dump "../rogue/src/synth2.s") => ok))



  
