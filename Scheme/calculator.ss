(load "some_parsers.ss")

(define (op2 c f)
    (! (symbol c) >> [\\ () (Parser_pure f)])
)

(define add (op2 #\+ +))
(define sub (op2 #\- -))
(define mul (op2 #\* *))
(define div (op2 #\/ /))
(define pow (op2 #\^ [\\ (x y) (exp (* y (log x)))]))

(define (guard b x)
    (if b (Parser_pure x) Parser_empty)
)

(define funcs (do! (n <- identifier) (<||>
    (guard (string=? n "sin")   sin)
    (guard (string=? n "cos")   cos)
    (guard (string=? n "asin")  asin)
    (guard (string=? n "acos")  acos)
    (guard (string=? n "sinh")  sinh)
    (guard (string=? n "cosh")  cosh)
    (guard (string=? n "asinh") asinh)
    (guard (string=? n "acosh") acosh)
    (guard (string=? n "tan")   tan)
    (guard (string=? n "atan")  atan)
    (guard (string=? n "log")   log)
    (guard (string=? n "exp")   exp)
    (guard (string=? n "sqrt")  sqrt)
    (guard (string=? n "sqr")   [\\ (x) (* x x)])
)))


(define consts (do! (n <- identifier) (<||>
    (guard (string=? n "E")        2.71828182845904523536)
    (guard (string=? n "PI")       3.14159265358979323846)
    (guard (string=? n "LOG2E")    1.44269504088896340736)  ; log2(e)
    (guard (string=? n "LOG10E")   0.434294481903251827651) ; log10(e)
    (guard (string=? n "LN2")      0.693147180559945309417) ; ln(2)
    (guard (string=? n "LN10")     2.30258509299404568402)  ; ln(10)
    (guard (string=? n "PI_2")     1.57079632679489661923)  ; pi/2
    (guard (string=? n "PI_4")     0.785398163397448309616) ; pi/4
    (guard (string=? n "1_PI")     0.318309886183790671538) ; 1/pi
    (guard (string=? n "2_PI")     0.636619772367581343076) ; 2/pi
    (guard (string=? n "2_SQRTPI") 1.12837916709551257390)  ; 2/sqrt(pi)
    (guard (string=? n "SQRT2")    1.41421356237309504880)  ; sqrt(2)
    (guard (string=? n "SQRT1_2")  0.707106781186547524401) ; 1/sqrt(2)
)))

(define (expr)
    (do!
        (sgn <- usign)
        (chainl1 (term) (! add <||> sub) (string=? sgn "-"))
    )
)

(define (term)
    (chainl1 (factor) (! mul <||> div) #f)
)

(define (factor)
    (chainr1 (factor0) pow)
)

(define (expr_in_brackets)
    (between (symbol #\( ) (symbol #\) ) expr)
)

(define (factor0)
    (<||>
        (expr_in_brackets)
        (! funcs <*> expr_in_brackets)
        consts
        double
    )
)

(define (calculate s)
    (parse (expr) s)
)
