; based on audio_dsp.xtm -- audio DSP library
; extempore - https://github.com/digego/extempore

(ns audio
  (include "math.h" "functional"))

(def PI 3.141592)
(def TWOPI (* 2.0 PI))
(def SAMPLERATE 44.100)

;; high limit
;; low limit
;; then value
(defn range-limit [^double h ^double l ^double v]
  (if (< v l) l
    (if (> v h) h
      v)))

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
(defn tap-delay-c [max-delay num-of-taps]
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
        y))))

;; allpass
(defn allpass-c [delay]
  (let [inline (array double delay)
	      outline (array double delay)
        time 0
        g 0.9]
    (fn [x]
      (let [n (% time delay)
            dy (pref outline n)
            dx (pref inline n)
            y (+ (* -1.0 g x)
                 dx
                 (* g dy))]
        (pset! inline n x)
        (pset! outline n y)
        (set! time (+ time 1))
        y))))

; (bind-func reverb_c
; TODO

;; a dodgy bitcrusher
(defn crusher-c [bits]
  (let [amp 1.0]
      (fn [in]
        (* amp (/ (floor (* in (pow 2.0 bits))) 
                  (pow 2.0 bits))))))

;; a dodgy amp distortion
(defn distort-c [gain]
  (let [lim 0.5]
    (fn [in]
      (range-limit lim (* -1.0 lim) (* gain in)))))

;; a four channel mixer
(defn mixquad [c1 c2 c3 c4 chan in]
  (cond (< chan 1.0) (* in c1)
        (< chan 2.0) (* in c2)
        (< chan 3.0) (* in c3)
        (< chan 4.0) (* in c4)
        :else 0.0))

; (bind-func mixer_c

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BiQuad coefficient formulae from 
;; Audio EQ Cookbook Robert Bristow-Johnson
;;
;; http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; biquad low-pass filter
(defn lpf-c []
  (let [y1 0.0
        y2 0.0
        x1 0.0
        x2 0.0
        b0 0.0
        b1 0.0
        b2 0.0
        a0 0.0
        a1 0.0
        a2 0.0
        res 0.25
        oldres 0.0
        oldfreq 0.0]
    (fn [x freq]
      ;; if frequency changes
      ;; recalculate coefficients
      (if (or (!= freq oldfreq)
              (!= res oldres))
        (let [omega (* TWOPI (/ freq SAMPLERATE))
              sino (sin omega)
              coso (cos omega)
              alpha (/ sino (* 2.0 res))]
          (set! oldfreq freq)
          (set! oldres res)
          (set! b0 (/ (- 1.0 coso) 2.0))
          (set! b1 (- 1.0 coso))
          (set! b2 b0)
          (set! a0 (+ 1.0 alpha))
          (set! a1 (* -2.0 coso))
          (set! a2 (- 1.0 alpha))))
      (let [y (- (+ (* (/ b0 a0) x)
                    (* (/ b1 a0) x1)
                    (* (/ b2 a0) x2))
                 (* (/ a1 a0) y1)
                 (* (/ a2 a0) y2))]
        (set! y2 y1)
        (set! y1 y)
        (set! x2 x1)
        (set! x1 x)
        y))))

;; biquad high-pass filter
(defn hpf-c []
  (fn []
    (let [y1 0.0
          y2 0.0
          x1 0.0
          x2 0.0
          b0 0.0
          b1 0.0
          b2 0.0
          a0 0.0
          a1 0.0
          a2 0.0
          res 0.25
          oldres 0.0
          oldfreq 0.0]
      (fn [x freq]
        ;; if frequency changes
        ;; recalculate coefficients
        (if (or (!= freq oldfreq)
                (!= res oldres))
          (let [omega (* TWOPI (/ freq SAMPLERATE))
                sino (sin omega)
                coso (cos omega)
                alpha (/ sino (* 2.0 res))]
            (set! oldfreq freq)
            (set! oldres res)
            (set! b0 (/ (+ 1.0 coso) 2.0))
            (set! b1 (* -1.0 (+ 1.0 coso)))
            (set! b2 b0)
            (set! a0 (+ 1.0 alpha))
            (set! a1 (* -2.0 coso))
            (set! a2 (- 1.0 alpha))))
        (let [y (- (+ (* (/ b0 a0) x)
                      (* (/ b1 a0) x1)
                      (* (/ b2 a0) x2))
                   (* (/ a1 a0) y1)
                   (* (/ a2 a0) y2))]
              (set! y2 y1)
              (set! y1 y)
              (set! x2 x1)
              (set! x1 x)
              y)))))

;; biquad band-pass filter
(defn bpf-c [] 
  (let [y1 0.0
        y2 0.0
        x1 0.0	   
        x2 0.0
        b0 0.0
        b1 0.0
        b2 0.0
        a0 0.0
        a1 0.0
        a2 0.0
        bandwidth 0.5
        oldfreq 0.0
        oldbw 0.0]
      ;; bandwidth in octaves
      (fn [x freq]
        ;; if frequency or bandwidth change
        ;; recalculate coefficients
        (if (or (!= freq oldfreq)
                (!= bandwidth oldbw))
          (let [omega (* 1.0 TWOPI (/ freq SAMPLERATE))
                sino (sin omega)
                coso (cos omega)
                alpha (* sino (sinh (* (/ (log2 2.0) 2.0)
                                       bandwidth
                                       (/ omega sino))))]
            (set! oldfreq freq)
            (set! oldbw bandwidth)
            (set! b0 alpha)
            (set! b1 0.0) 
            (set! b2 (* -1.0 b0))
            (set! a0 (+ 1.0 alpha))
            (set! a1 (* -2.0 coso))
            (set! a2 (- 1.0 alpha))))
        (let [y (- (+ (* (/ b0 a0) x)
                      (* (/ b1 a0) x1)
                      (* (/ b2 a0) x2))
                   (* (/ a1 a0) y1)
                   (* (/ a2 a0) y2))]
          (set! y2 y1)
          (set! y1 y)
          (set! x2 x1)
          (set! x1 x)
          y))))

;; biquad notch filter
(defn notch-c []
    (let [y1 0.0
          y2 0.0
          x1 0.0
          x2 0.0
          b0 0.0
          b1 0.0
          b2 0.0
          a0 0.0
          a1 0.0
          a2 0.0	     
          bandwidth 0.5 ; in ocatves
          oldfreq 0.0
          oldbw 0.0]
      ;; bandwidth in octaves
      (fn [x freq]
        ;; if frequency or bandwidth change
        ;; recalculate coefficients
        (if (or (!= freq oldfreq)
                (!= bandwidth oldbw))
          (let [omega (* TWOPI (/ freq SAMPLERATE))
                sino (sin omega)
                coso (cos omega)
                alpha (* sino (sinh (* (/ (log2 2.0) 2.0)
                                       bandwidth
                                       (/ omega sino))))]
            (set! oldfreq freq)
            (set! oldbw bandwidth)
            (set! b0 1.0)
            (set! b1 (* -2.0 coso)) 
            (set! b2 b0)
            (set! a0 (+ 1.0 alpha))
            (set! a1 b1)
            (set! a2 (- 1.0 alpha))))
        (let [y (- (+ (* (/ b0 a0) x)
                      (* (/ b1 a0) x1)
                      (* (/ b2 a0) x2))
                   (* (/ a1 a0) y1)
                   (* (/ a2 a0) y2))]
          (set! y2 y1)
          (set! y1 y)
          (set! x2 x1)
          (set! x1 x)
          y))))

;; Hilbert transform filter
(defn hilbert-c
  "n = filter order (should be 2^n + 1)"
  [n]
  (let [h_n (array double n)
        x_n (array double n)
        x_ptr 0]
    ;; h[i] = (2/(i*pi))*sin^2((i*pi)/2) for i=-n/2,...,n/2
    (dotimes [i (/ (* n -1) 2)]
      (pset! h_n
             (+ i (/ n 2))
             (* (/ 2.0 (* (double i) PI))
                (pow (sin (/ (* (double i) PI) 2.0)) 2.0))))
    ;; h[0] = 0
    (pset! h_n (+ 1 (/ n 2)) 0.0)
    (fn [x]
      (pset! x_n x_ptr x)
      (let [out 0.0]
        (dotimes [i n]
          (set! out (+ out (* (pref h_n i)
                              (pref x_n (% (+ i x_ptr (- n 1)) n))))))
        (set! x_ptr (% (+ x_ptr 1) n))
        out))))

(defn env-followc [n]
  (let [hilb (hilbert-c n)]
    (fn [x]
      (sqrt (+ (pow x 2.0)
               (pow (hilb x) 2.0))))))

;;
;; moog VCF
;;
;; from Stilson/Smith CCRMA
;;
(defn vcf-c []
  (let [res 0.5 ;; 0.0 - 1.0
        x 0.0 y1 0.0 y2 0.0 y3 0.0 y4 0.0
        oldx 0.0 oldy1 0.0 oldy2 0.0 oldy3 0.0]
    (fn [in cutoff]
      (let [f (* 1.75 (/ cutoff SAMPLERATE))
            k (- (- (* 3.6 f) (* 1.6 (* f f))) 1.0)
            p (* 0.5 (+ k 1.0))
            scale (exp (* (- 1.0 p) 1.386249))
            r (* res scale)]
        (set! x (- in (* r y4)))
        (set! y1 (+ (* x  p) (* oldx  p) (* -1.0 k y1)))
        (set! y2 (+ (* y1 p) (* oldy1 p) (* -1.0 k y2)))
        (set! y3 (+ (* y2 p) (* oldy2 p) (* -1.0 k y3)))
        (set! y4 (+ (* y3 p) (* oldy3 p) (* -1.0 k y4)))        
        (set! oldx x) (set! oldy1 y1) (set! oldy2 y2) (set! oldy3 y3)
        ;; y4 is output
        (set! y4 (- y4 (/ (pow y4 3.0) 6.0)))
        y4))))

;;
;; moog VCF v2.0
;;
;; from Stilson/Smith CCRMA
;;
(defn vcf2-c []
  (let [res 0.5 ;; 0.0 - 1.0
        in1 0.0 in2 0.0 in3 0.0 in4 0.0
        out1 0.0 out2 0.0 out3 0.0 out4 0.0]
    (fn [in cutoff]
      (let [f (/ (* 7.0 cutoff) (* 1.16 SAMPLERATE)) ;1.16))
            f1 (- 1.0 f)
            fb (* res 4.0 (- 1.0 (* 0.15 f f)))]
        (set! in (- in (* out4 fb)))
        (set! in (* in 0.35013 f f f f))
        (set! out1 (+ in   (* 0.3 in1) (* f1 out1))) ;; Pole 1
        (set! in1 in)
        (set! out2 (+ out1 (* 0.3 in2) (* f1 out2)))  ;; Pole 2	  
        (set! in2 out1)
        (set! out3 (+ out2 (* 0.3 in3) (* f1 out3)))  ;; Pole 3
        (set! in3 out2)
        (set! out4 (+ out3 (* 0.3 in4) (* f1 out4)))  ;; Pole 4	  
        (set! in4 out3)
        out4))))

;; based on Steve Harris's foverdrive lv2 plugin
;; http://plugin.org.uk/

(defn overdrive-c
  "Drive range:[1.0,3.0]"
  [drive]
  (let [drive_minus_1 (- drive 1.0)]
    (fn [x]
      (let [absx (fabs x)]
        (/ (* x (+ absx drive))
           (+ (* x x)
              (* drive_minus_1 absx)
              1.0))))))

;; based on Steve Harris's Valve Saturation lv2 plugin
;; http://plugin.org.uk/

(defn saturation-c
  "level:[0.0,1.0] character:[0.0,1.0]"
  []
  (let [itm1 0.0
        otm1 0.0]
    (fn [x level character]
      (let [q (- level 0.999)
            dist (+ (* character 40.0) 0.1)
            fx (if (= x q)
                 (/ (/ 1.0 (+ dist q))
                    (- 1.0 (exp (* dist q))))
                 (/ (/ (- x q)
                       (+ (- 1.0 (exp (* -1.0 dist (- x q)))) q))
                    (- 1.0 (exp (* dist q)))))]
        (set! otm1 (- (+ (* 0.999 otm1) fx) itm1))
        (set! itm1 fx)
        otm1))))

(defn ringmod-c
  "freq feedback:[0.0,1.0]"
  []
  (let [osc (osc-c 0.0)]
    (fn [x freq feedback]
      (+ (* (- 1.0 feedback) x)
         (* feedback (* x (osc freq 1.0)))))))

;; time in samples (starting from 0)
;; apex (in samples) is after how many samples we hit 1.0 before decaying
(defn impulse [time apex]
  (let [h (* time apex)]
    (* h (exp (- 1.0 h)))))


;;  midi/freq utilities

(defn midi2frq [pitch]            
  (* 440.0 (pow 2.0 (/ (- pitch 69.0) 12.0))))

(defn frq2midi [freq]            
  (+ (* 12.0 (log2 (/ freq 440.0))) 69.0))

