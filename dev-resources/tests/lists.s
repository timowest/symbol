(ns lists.s
  (include "list"))

(defn create []
  (let [l (new (std/list int))]
    (.push_back l 0i)
    l))