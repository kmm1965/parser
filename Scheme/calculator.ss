(load "some_parsers.ss")

(define (sqr x) (* x x))

(define (def_object n x)
    (! (name n) >> (Parser_pure x))
)

(define func (<||>
    (def_object "sin"   sin)
    (def_object "cos"   cos)
    (def_object "asin"  asin)
    (def_object "acos"  acos)
    (def_object "sinh"  sinh)
    (def_object "cosh"  cosh)
    (def_object "asinh" asinh)
    (def_object "acosh" acosh)
    (def_object "tan"   tan)
    (def_object "atan"  atan)
    (def_object "log"   log)
    (def_object "exp"   exp)
    (def_object "sqrt"  sqrt)
    (def_object "sqr"   sqr)
))


(define m_E        2.71828182845904523536)   ; e
(define m_LOG2E    1.44269504088896340736)   ; log2(e)
(define m_LOG10E   0.434294481903251827651)  ; log10(e)
(define m_LN2      0.693147180559945309417)  ; ln(2)
(define m_LN10     2.30258509299404568402)   ; ln(10)
(define m_PI       3.14159265358979323846)   ; pi
(define m_PI_2     1.57079632679489661923)   ; pi/2
(define m_PI_4     0.785398163397448309616)  ; pi/4
(define m_1_PI     0.318309886183790671538)  ; 1/pi
(define m_2_PI     0.636619772367581343076)  ; 2/pi
(define m_2_SQRTPI 1.12837916709551257390)   ; 2/sqrt(pi)
(define m_SQRT2    1.41421356237309504880)   ; sqrt(2)
(define m_SQRT1_2  0.707106781186547524401)  ; 1/sqrt(2)

(define const (<||>
    (def_object "E"        m_E)
    (def_object "LOG2E"    m_LOG2E)
    (def_object "LOG10E"   m_LOG10E)
    (def_object "LN2"      m_LN2)
    (def_object "LN10"     m_LN10)
    (def_object "PI"       m_PI)
    (def_object "PI_2"     m_PI_2)
    (def_object "PI_4"     m_PI_4)
    (def_object "1_PI"     m_1_PI)
    (def_object "2_PI"     m_2_PI)
    (def_object "2_SQRTPI" m_2_SQRTPI)
    (def_object "SQRT2"    m_SQRT2)
    (def_object "SQRT1_2"  m_SQRT1_2)
))

(define (op2 c f)
    (! (symbol c) >> (Parser_pure f))
)

(define add (op2 #\+ +))
(define sub (op2 #\- -))
(define mul (op2 #\* *))
(define div (op2 #\/ /))
(define pow (op2 #\^ [\\ (x y) (exp (* y (log x)))]))

(define (expr)
    (chainl1 (term) (! add <||> sub))
)

(define (term)
    (chainl1 (factor) (! mul <||> div))
)

(define (factor)
    (chainr1 (factor0) pow)
)

(define (expr_in_brackets)
    (define br_open  (symbol #\( ))
    (define br_close (symbol #\) ))
    
    (between br_open br_close expr)
)

(define (factor0)
    (<||>
        (expr_in_brackets)
        (! func <*> expr_in_brackets)
        const
        double
    )
)

(define (calculate s)
    (parse (expr) s)
)
