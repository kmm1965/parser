(load "calculator.ss")

(printf "~a\n" (calculate " 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"))
(printf "~a\n" (calculate "sin ( 2_SQRTPI * sqr ( 2 ) - 1 )"))
(printf "~a\n" (calculate "sqr(sin(2)) + sqr(cos(1 + 1))"))
(printf "~a\n" (calculate "3 ^ 2 ^ 3"))
(printf "~a\n" (calculate " E ^ PI "))
(printf "~a\n" (calculate " PI ^ E "))
