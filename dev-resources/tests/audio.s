(ns osc
  (include "math.h" "functional"))

(def PI 3.141592)
(def SAMPLERATE 44.100)

(defn osc-c [phase]
  (fn [amp freq]
    (let [inc (* 2.0 PI (/ freq SAMPLERATE))]
      (set! phase (+ phase inc))
      (* amp (sin phase)))))

;; square oscillator
(defn square-c [phase]
  (let [osc (osc-c phase)
        n 50.0]
    (fn [amp freq]
      (* amp (tanh (* n (osc 1.0 freq)))))))

;; rect wave oscillator - useful for pulse width modulation
(defn rect-c [phase]
  (fn [amp freq duty]
    (let [inc (/ freq SAMPLERATE)]
      (set! phase (+ phase inc))
      (when (> phase 1.0) (set! phase (- phase 1.0)))
      (if (< phase duty) amp  (* -1.0 amp)))))

;; saw oscillator
(defn saw-c []
  (let [p 0.0
        dp 1.0
        x 0.0
        leak 0.995
        saw 0.0]
    (fn [amp freq]
      (let [qmax (* 0.5 (/ SAMPLERATE freq))
            dc (/ -0.498 qmax)]
        (set! p (+ p dp))
        (when (< p 0.0) 
          (set! p (- 0.0 p))
          (set! dp (- 0.0 dp)))
        (when (> p qmax)
          (set! p (+ qmax (- qmax p)))
          (set! dp (- 0.0 dp)))
        (set! x (* PI p))
        (if (< x 0.000001) (set! x 0.00001))
        (set! saw (* leak (+ saw (+ dc (/ (sin x) x)))))
        (* amp saw)))))

;; white noise generator
;(bind-func white_c
; TODO 

;; pink noise generator
;; uses Paul Kellet's economy method
;; http://www.firstpr.com.au/dsp/pink-noise/
;(bind-func pink_c
; TODO

;; pulse train
(defn pulse-c []
  (let [time -1.0
        width 100.0]
    (fn [amp freq]
      (let [period (/ SAMPLERATE freq)]
        (set! time (+ time 1.0))
        (if (< (mod time period) width)
          amp
          0.0)))))

; (bind-func delay_c

; (bind-func comb_c

; (bind-func flanger_c

; (bind-func chorus_c

; (bind-func tap_delay_c

; (bind-func allpass_c

; (bind-func reverb_c

; (bind-func crusher_c

; (bind-func distort_c

; (bind-func mixquad

; (bind-func mixer_c

; (bind-func lpf_c

; (bind-func hpf_c

; (bind-func bpf_c

; (bind-func notch_c

; (bind-func hilbert_c

; (bind-func env_follow_c

; (bind-func vcf_c

; (bind-func vcf2_c

; (bind-func overdrive_c

; (bind-func saturation_c

; (bind-func ringmod_c

; bind-func impulse
