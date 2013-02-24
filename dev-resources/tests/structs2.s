(ns structs2)

(deftype _Struct [^int a ^int b])

(defn get-a [^_Struct* s]
  (.a s))

(defn get-b [^_Struct s]
  (.b s))