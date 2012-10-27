(ns symbol.emission
  (:require [clojure.pprint :as pprint]))

; if fn* let* loop* recur* . new def do 
; set! pset! pref 
; < > <= >= + - * /

(def cpp-types
  '{boolean boolean
    char   int8_t
    short  int16_t
    int    int32_t
    long   int64_t
    uchar  uint8_t
    ushort uint16_t
    uint   uint32_t
    ulong  uint64_t})

(defn get-type
  [env form]
  (let [matches (keep (fn [[f t]] (if (= f form) t)) env)]
    (first matches)))

(defn emit 
  [env target form]
  (pprint/pprint form)) 