(ns symbol.emission)

; C++ emission

; if fn* let* loop* recur* . new def do 
; set! pset! pref 
; < > <= >= + - * /

; char   int8_t
; short  int16_t
; int    int32_t
; long   int64_t
; uchar  uint8_t
; ushort uint16_t
; uint   uint32_t
; ulong  uint64_t

; defn emit [env target form]
