(ns pointers2)

; XXX is array-of a good name, create might be too generic?
(defmacro array-of [n expr]
  (let [type (gensym "_")]
    `(let [arr# (array ~type ~n)]
       (dotimes [i# ~n]
         (pset! arr# i# ~expr))
     arr#)))

(deftype Osc [^int type ^double phase ^double freq])

(deftype Lfo [^int type ^double phase ^double freq])

(deftype Filter [^int type ^double freq ^double q])

(deftype Envelope [^int type]) 

(defn tests []
  (let [oscs (array-of 4 (Osc. 0i 0.0 0.0))
        lfos (array-of 4 (Lfo. 0i 0.0 0.0))
        filters (array-of 4 (Filter. 0i 0.0 0.0))
        envs (array-of 4 (Envelope. 0i))]
    0i))
  
  