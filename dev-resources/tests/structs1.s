(ns structs1)

(defstruct Synth
  (sample-rate double)
  (freq float)
  (output float))

(defn create-and-delete []
  (let [a (Synth.)]
    (delete a)))

(defn create-and-access []
  (let [a (Synth.)
        b (.freq a)]
    (delete a)))

(defn create-and-update []
  (let [a (Synth.)]
    (set! (.freq a) (float 0.0))
    (delete a)))