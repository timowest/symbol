(ns lists.s
  (include "list"))

(defn create []
  (let [l (new (std/list int))]
    (.push_back l 0i)
    l))

; TODO extend type wrapping ?

(defn first [^{:tag (pointer (std/list _0))} l]
  (. l (front)))

(defn last  [^{:tag (pointer (std/list _0))} l]
  (. l (back)))

; TODO nth

; TODO cons
