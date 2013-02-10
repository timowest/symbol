(ns midi.s)

(defn midi [^uchar* data ^uint size]
  (let [status (& (pref data 0) 0xf0)
        key    (& (pref data 1) 0x7f)
        value  (& (pref data 2) 0x7f)]
    (cond
      ; note on
      (and (= status 0x90) (> value 0))        
        1 ; find voice and trigger attack
      ; note off
      (or (= status 0x80) (and (= status 0x90) (= value 0)))         
        2 ; trigger release     
      ; control change
      (= status 0xb0)
        (match key
               0x01 1 ; modulation wheel
               0x07 2 ; channel volume
               0x0a 3 ; channel panning
               0x78 4 ; all sound off
               0x79 5 ; all controllers off
               0x7b 6) ; all notes off
      ; pitchbend
      (= status 0xe0)
      4)))
      