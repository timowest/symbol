(ns synth
  (include "math.h" "functional"))

(def PI 3.141592)
(def TWOPI (* 2.0 PI))
(def SAMPLERATE 44.100)

(def INVALID-KEY 255i)

(defn midi2frq [pitch]            
  (* 440.0 (pow 2.0 (/ (- pitch 69.0) 12.0))))

(defn frq2midi [freq]            
  (+ (* 12.0 (log2 (/ freq 440.0))) 69.0))

(deftype Data [^double sample-rate ^float* left ^float* right])

(deftype Osc [^int type ^double phase ^double freq]
  
  (run [_]
    (let [inc (* TWOPI (/ freq SAMPLERATE))]
      (set! phase (+ phase inc))
      (when (> phase 1.0)
        (set! phase (- phase 1.0)))
      phase)))

; TODO at first only sine oscillator

(deftype Lfo [^int type ^double phase ^double freq] ; TODO
  
  (run [_]))

(deftype Filter [^int type ^double freq ^double q] ; TODO
  
  (run [_]))

(deftype Envelope [^int type] ; TODO envelope state
  
  (run [_]))

; TODO key, gain
(deftype Voice [^Data* data ^Osc** oscs ^Lfo** lfos ^Filter** filters ^Envelope** envelopes]
  
  (on [_ _key _velocity]
    (set! key _key)
    (set! velocity _velocity))  
  
  (off [_ _velocity]
    (set! key INVALID-KEY))
  
  (run [_ from to]
    (when (!= key INVALID-KEY)
      (dorange [i from to]
        (let [val (float (run (nth oscs 0)))]
          (pset! (.left data) i val)
          (pset! (.right data) i val))))))
  
(defn new-voice [data]
  (let [oscs (array-of 4 (Osc. 0i 0.0 0.0))
        lfos (array-of 4 (Osc. 0i 0.0 0.0))
        filters (array-of 4 (Filter. 0i 0.0 0.0))
        envs (array-of 4 (Envelope. 0i))]
    (Voice. data oscs lfos filters envelopes)))

(deftype Synth [^Data* data ^long num-voices ^Voice** voices]
  
  (find-free-voice [_ key]
    (loop [i 0i]
      (cond (= (.key (nth voices i)) INVALID-KEY) i
            (< i (- num-voices 1i)) (recur (+ i 1i))
            :else 0i)))
  
  (note-on [_ key velocity]
    (let [idx (find-free-voice _ key)]
      (on (nth voices idx) key velocity)))
  
  (note-off [_ key velocity]
    (dotimes [i num-voices]
      (when (= (.key (nth voices i)) key)
        (off (nth voices i) velocity))))
  
  (cc-change [_ id val]) ; TODO
  
  (pitch-bend [_ velocity]) ; TODO
  
  (pre [_ from to]
    (dorange [i from to]
      (pset! (.left data) i 0.0f)
      (pset! (.right data) i 0.0f)))
  
  (post [_ from to]) ; TODO                   
          
  (run [_ from to]
    (dotimes [i num-voices]
      (run (nth voices i) from to))))

(defn new-synth [num-voices data]
  (let [voices (array-of num-voices (create-voice data))]
    (Synth. data num-voices voices)))
    
  