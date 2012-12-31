(ns audio
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
      (if (< phase duty) amp (* -1.0 amp)))))


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
        (if (< (fmod time period) width) 
          amp
          0.0)))))

;; iir comb without interpolation
;; more efficient than comb if you
;; don't need variable length
(defn delay-c [max-delay]
  (let [line (array double max-delay)
	      time 0
	      delay max-delay
	      in 0.5
	      out 0.5]
    (fn [x]
      (let [n (% time delay)
            delayed (pref line n)
            y (+ (* in x) (* out delayed))]
        (pset! line n y)
        (set! time (+ time 1))
        y))))

;; iir comb with interpolation
(defn comb-c [max-delay]
  (let [line (array double max-delay)
	      in-head 0
	      out-head 0
	      delay_ (double max-delay)
	      delay (double max-delay)
	      alpha 0.0
	      om-alpha 1.0
	      in 1.0
	      out 0.5]
      (dotimes [i max-delay] 
        (pset! line i 0.0))
      (fn [x]
        (when (!= delay delay_)
          (set! delay_ delay)		 
          (set! alpha (- delay (floor delay)))
          (set! om-alpha (- 1.0 alpha))
          (set! out-head (- (+ max-delay in-head)
                            (long delay))))
        (let [ih (% in-head max-delay)
              oh (% out-head max-delay)
              delayed1 (pref line oh)
              delayed2 (pref line (% (+ oh 1) max-delay))
              delayed (+ (* alpha delayed1) (* om-alpha delayed2)) 
              y (+ (* in x) (* out delayed))]
          (pset! line ih y)
          (set! in-head (+ ih 1))
          (set! out-head (+ oh 1))
          y))))

; (bind-func flanger_c
; TODO

; (bind-func chorus_c
; TODO

;; tap delay
(comment (defn tap-delay-c [max-delay num-of-taps]
  (let [line (array double max-delay)
	      taps (array long num-of-taps)
	      delay max-delay
	      time 0]
    (fn [x]
      (let [y 0.0
            n (% time delay)
            gain (/ 1.0 (double num-of-taps))]
        (pset! line n x)
        (dotimes [i num-of-taps]
          (set! y (+ y (* gain (pref line (% (+ (pref taps i) n) delay))))))
        (set! time (+ time 1))
        y)))))

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
