(ns includes
  (include "regex.h"))

(defn regex-buffer1 [^regex_t* arg]
  (.buffer arg))

(defn regex-buffer2 [^re_pattern_buffer* arg]
  (.buffer arg))

(defn regex-buffer3 [arg]
  (.buffer arg))