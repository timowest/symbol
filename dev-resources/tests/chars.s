(ns chars
  (include "string.h"))

(defn strcmp2 [ch]
  (= (strcmp ch "abc") 0i))