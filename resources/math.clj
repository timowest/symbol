(ns math)

(def cos ^(fn [double] double) :native)
(def cos ^(fn [float] float) :native)
(def cos ^(fn [quad] quad) :native)

(def sin ^(fn [double] double) :native)
(def sin ^(fn [float] float) :native)
(def sin ^(fn [quad] quad) :native)

(def tan ^(fn [double] double) :native)
(def tan ^(fn [float] float) :native)
(def tan ^(fn [quad] quad) :native)

(def acos ^(fn [double] double) :native)
(def acos ^(fn [float] float) :native)
(def acos ^(fn [quad] quad) :native)

(def asin ^(fn [double] double) :native)
(def asin ^(fn [float] float) :native)
(def asin ^(fn [quad] quad) :native)

(def atan ^(fn [double] double) :native)
(def atan ^(fn [float] float) :native)
(def atan ^(fn [quad] quad) :native)

(def atan2 ^(fn [double] double) :native)
(def atan2 ^(fn [float] float) :native)
(def atan2 ^(fn [quad] quad) :native)

(def atan ^(fn [double] double) :native)
(def atan ^(fn [float] float) :native)
(def atan ^(fn [quad] quad) :native)

(def atan2 ^(fn [double] double) :native)
(def atan2 ^(fn [float] float) :native)
(def atan2 ^(fn [quad] quad) :native)

(def cosh ^(fn [double] double) :native)
(def cosh ^(fn [float] float) :native)
(def cosh ^(fn [quad] quad) :native)

(def sinh ^(fn [double] double) :native)
(def sinh ^(fn [float] float) :native)
(def sinh ^(fn [quad] quad) :native)

(def tanh ^(fn [double] double) :native)
(def tanh ^(fn [float] float) :native)
(def tanh ^(fn [quad] quad) :native)

(def exp ^(fn [double] double) :native)
(def exp ^(fn [float] float) :native)
(def exp ^(fn [quad] quad) :native)

(def frexp ^(fn [double int*] double) :native)
(def frexp ^(fn [float int*] float) :native)
(def frexp ^(fn [quad int*] quad) :native)

(def ldexp ^(fn [double int] double) :native)
(def ldexp ^(fn [float int] float) :native)
(def ldexp ^(fn [quad int] quad) :native)

(def log ^(fn [double] double) :native)
(def log ^(fn [float] float) :native)
(def log ^(fn [quad] quad) :native)

(def log10 ^(fn [double] double) :native)
(def log10 ^(fn [float] float) :native)
(def log10 ^(fn [quad] quad) :native)

(def modf ^(fn [double double*] double) :native)
(def modf ^(fn [float float*] float) :native)
(def modf ^(fn [quad quad*] quad) :native)

(def pow ^(fn [double double] double) :native)
(def pow ^(fn [quad quad] quad) :native)
(def pow ^(fn [float float] float) :native)
(def pow ^(fn [double int] double) :native)
(def pow ^(fn [quad int] quad) :native)

(def sqrt ^(fn [double] double) :native)
(def sqrt ^(fn [float] float) :native)
(def sqrt ^(fn [quad] quad) :native)

(def ceil ^(fn [double] double) :native)
(def ceil ^(fn [float] float) :native)
(def ceil ^(fn [quad] quad) :native)

(def fabs ^(fn [double] double) :native)
(def fabs ^(fn [float] float) :native)
(def fabs ^(fn [quad] quad) :native)

(def floor ^(fn [double] double) :native)
(def floor ^(fn [float] float) :native)
(def floor ^(fn [quad] quad) :native)

(def fmod ^(fn [double double] double) :native)
(def fmod ^(fn [float float] float) :native)
(def fmod ^(fn [quad quad] quad) :native)
