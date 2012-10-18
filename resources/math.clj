(ns math)

(def cos ^{:tag (fn [double] double)} 'native)
(def cos ^{:tag (fn [float] float)} 'native)
(def cos ^{:tag (fn [quad] quad)} 'native)

(def sin ^{:tag (fn [double] double)} 'native)
(def sin ^{:tag (fn [float] float)} 'native)
(def sin ^{:tag (fn [quad] quad)} 'native)

(def tan ^{:tag (fn [double] double)} 'native)
(def tan ^{:tag (fn [float] float)} 'native)
(def tan ^{:tag (fn [quad] quad)} 'native)

(def acos ^{:tag (fn [double] double)} 'native)
(def acos ^{:tag (fn [float] float)} 'native)
(def acos ^{:tag (fn [quad] quad)} 'native)

(def asin ^{:tag (fn [double] double)} 'native)
(def asin ^{:tag (fn [float] float)} 'native)
(def asin ^{:tag (fn [quad] quad)} 'native)

(def atan ^{:tag (fn [double] double)} 'native)
(def atan ^{:tag (fn [float] float)} 'native)
(def atan ^{:tag (fn [quad] quad)} 'native)

(def atan2 ^{:tag (fn [double] double)} 'native)
(def atan2 ^{:tag (fn [float] float)} 'native)
(def atan2 ^{:tag (fn [quad] quad)} 'native)

(def atan ^{:tag (fn [double] double)} 'native)
(def atan ^{:tag (fn [float] float)} 'native)
(def atan ^{:tag (fn [quad] quad)} 'native)

(def atan2 ^{:tag (fn [double] double)} 'native)
(def atan2 ^{:tag (fn [float] float)} 'native)
(def atan2 ^{:tag (fn [quad] quad)} 'native)

(def cosh ^{:tag (fn [double] double)} 'native)
(def cosh ^{:tag (fn [float] float)} 'native)
(def cosh ^{:tag (fn [quad] quad)} 'native)

(def sinh ^{:tag (fn [double] double)} 'native)
(def sinh ^{:tag (fn [float] float)} 'native)
(def sinh ^{:tag (fn [quad] quad)} 'native)

(def tanh ^{:tag (fn [double] double)} 'native)
(def tanh ^{:tag (fn [float] float)} 'native)
(def tanh ^{:tag (fn [quad] quad)} 'native)

(def exp ^{:tag (fn [double] double)} 'native)
(def exp ^{:tag (fn [float] float)} 'native)
(def exp ^{:tag (fn [quad] quad)} 'native)

(def frexp ^{:tag (fn [double int*] double)} 'native)
(def frexp ^{:tag (fn [float int*] float)} 'native)
(def frexp ^{:tag (fn [quad int*] quad)} 'native)

(def ldexp ^{:tag (fn [double int] double)} 'native)
(def ldexp ^{:tag (fn [float int] float)} 'native)
(def ldexp ^{:tag (fn [quad int] quad)} 'native)

(def log ^{:tag (fn [double] double)} 'native)
(def log ^{:tag (fn [float] float)} 'native)
(def log ^{:tag (fn [quad] quad)} 'native)

(def log10 ^{:tag (fn [double] double)} 'native)
(def log10 ^{:tag (fn [float] float)} 'native)
(def log10 ^{:tag (fn [quad] quad)} 'native)

(def modf ^{:tag (fn [double double*] double)} 'native)
(def modf ^{:tag (fn [float float*] float)} 'native)
(def modf ^{:tag (fn [quad quad*] quad)} 'native)

(def pow ^{:tag (fn [double double] double)} 'native)
(def pow ^{:tag (fn [quad quad] quad)} 'native)
(def pow ^{:tag (fn [float float] float)} 'native)
(def pow ^{:tag (fn [double int] double)} 'native)
(def pow ^{:tag (fn [quad int] quad)} 'native)

(def sqrt ^{:tag (fn [double] double)} 'native)
(def sqrt ^{:tag (fn [float] float)} 'native)
(def sqrt ^{:tag (fn [quad] quad)} 'native)

(def ceil ^{:tag (fn [double] double)} 'native)
(def ceil ^{:tag (fn [float] float)} 'native)
(def ceil ^{:tag (fn [quad] quad)} 'native)

(def fabs ^{:tag (fn [double] double)} 'native)
(def fabs ^{:tag (fn [float] float)} 'native)
(def fabs ^{:tag (fn [quad] quad)} 'native)

(def floor ^{:tag (fn [double] double)} 'native)
(def floor ^{:tag (fn [float] float)} 'native)
(def floor ^{:tag (fn [quad] quad)} 'native)

(def fmod ^{:tag (fn [double double] double)} 'native)
(def fmod ^{:tag (fn [float float] float)} 'native)
(def fmod ^{:tag (fn [quad quad] quad)} 'native)
