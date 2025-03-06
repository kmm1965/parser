; String support
(define (string-head s)
    (car (string->list s))
)

(define (string-tail s)
    (substring s 1 (string-length s))
)

; Maybe Support
(define (Just x) (list x))
(define (Nothing) '())

(define (Maybe_do m on_Just)
    (if (> (length m) 0) (on_Just) (Nothing))
)

; Functor
(define (Maybe_fmap f m)
    (Maybe_do m [lambda () (Just (f (car m)))])
)

(define (Maybe_fmapr m f)
    (Maybe_fmap f m)
)

; Monad
(define (Maybe_and_then m f)
    (Maybe_do m [lambda () (f (car m))])
)

; Applicative
(define (Maybe_pure x) (Just x))

(define (Maybe_apply mf m)
    (Maybe_and_then mf [lambda (f) (Maybe_fmap f m)])
)

; Alternative
(define (Maybe_or_else m1 m2)
    (if (> (length m1) 0) m1 m2)
)

(define  (Maybe_or_else_get m1 fm2)
    (if (> (length m1) 0) m1 (fm2))
)

(define (Parser_parse p inp)
    (p inp)
)

; Functor
(define (Parser_fmap f p)
    [lambda (inp)
        (Maybe_fmapr (Parser_parse p inp)
            [lambda (pair)
                (list (f (car pair)) (cadr pair))
            ]
        )
    ]
)

(define (Parser_fmapr p f) (Parser_fmap f p))

; Monad
(define (Parser_and_then p f)
    [lambda (inp)
        (Maybe_and_then (Parser_parse p inp)
            [lambda (pair) (Parser_parse (f (car pair)) (cadr pair))]
        )
    ]
)

(define (Parser_skip p q)
    (Parser_and_then p [lambda (_) q])
)

(define (Parser_skip_get p fq)
    (Parser_and_then p [lambda (_) (fq)])
)

; Applicative
(define (Parser_pure x)
    [lambda (inp) (Just (list x inp))]
)

(define (Parser_apply pf fq)
    (Parser_and_then pf [lambda (f) (Parser_fmap f (fq))])
)

; Alternative
(define (Parser_empty)
    [lambda (_) (Nothing)]
)

(define (Parser_or_else . ps)
    [lambda (inp)
        (fold-left
            [lambda (m p) (Maybe_or_else_get m [lambda () (Parser_parse p inp)])]
            (Nothing) ps
        )
    ]
)

(define (Parser_or_else_get p fq)
    [lambda (inp)
        (Maybe_or_else_get
            (Parser_parse p inp)
            [lambda () (Parser_parse (fq) inp)]
        )
    ]
)

(define (anyChar)
    [lambda (inp)
        (if (> (string-length inp) 0)
            (Just (list (string-head inp) (string-tail inp)))
            (Nothing)
        )
    ]
)

(define (satisfy pred?)
    (Parser_and_then (anyChar)
        [lambda (c)
            (if (pred? c)
                (Parser_pure c)
                (Parser_empty)
            )
        ]
    )
)

(define (char c)
    (satisfy [lambda (x) (eq? x c)])
)

(define (some p)
    (Parser_apply
        (Parser_fmapr p [lambda (c) [lambda (s) (string-append (string c) s)]])
        [lambda () (many p)]
    )
)

(define (many p)
    (Parser_or_else_get (some p) [lambda () (Parser_pure "")])
)

(define (alnum)
    (satisfy [lambda (c) (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_))])
)

(define (spaces)
    (many (satisfy char-whitespace?))
)

(define (token p)
    (Parser_and_then
        (Parser_skip (spaces) p)
        [lambda (a) (Parser_skip (spaces) (Parser_pure a))]
    )
)

(define (symbol x)
    (token (satisfy [lambda (c) (char=? c x)]))
)

(define (name n)
    (token (Parser_and_then
        (some (alnum))
        [lambda (s)
            (if (string=? s n) (Parser_pure n) (Parser_empty))
        ]
    ))
)

(define (optional_s p)
    (Parser_or_else_get p [lambda () (Parser_pure "")])
)

(define (optional_c p)
    (optional_s
        (Parser_fmap [lambda (c) (string c)] p)
    )
)

(define (digits)
    (many (satisfy char-numeric?))
)

(define (sign)
    (optional_c (Parser_or_else (char #\+) (char #\-)))
)

(define (double_)
    (token (Parser_and_then (some (satisfy char-numeric?))
        [lambda (s) (Parser_pure (string->number s))]
    ))
)

(define (double)
    (token (Parser_and_then (sign)
        [lambda (sign_part) (Parser_and_then (digits)
            [lambda (int_part) (Parser_and_then
                (optional_s (Parser_skip_get (char #\.) digits))
                [lambda (frac_part) (Parser_and_then
                    (optional_s (Parser_and_then
                        (Parser_skip_get
                            (Parser_or_else (char #\e) (char #\E))
                            sign
                        )
                        [lambda (exp_sign) (Parser_and_then
                            (some (satisfy char-numeric?))
                            [lambda (exp_digits)
                                (Parser_pure (string-append exp_sign exp_digits))
                            ]
                        )]
                    ))
                    [lambda (exp_part) (if
                        (or
                            (> (string-length int_part) 0)
                            (> (string-length frac_part) 0)
                        )
                        (Parser_pure (string->number
                            (string-append
                                sign_part int_part
                                (if (> (string-length frac_part) 0)
                                    (string-append "." frac_part)
                                    ""
                                )
                                (if (> (string-length exp_part) 0)
                                    (string-append "e" exp_part)
                                    ""
                                )
                            )
                        ))
                        (Parser_empty)
                    )]
                )]
            )]
        )]
    ))
)

(define (rest p ff op x)
    (Parser_or_else_get
        (Parser_and_then op
            [lambda (f) (Parser_and_then p
                [lambda (y) (ff (f x y))]
            )]
        )
        [lambda () (Parser_pure x)]
    )
)

(define (chainl1 p op)
    (define (rest_l x) (rest p rest_l op x))

    (Parser_and_then p [lambda (x) (rest_l x)])
)

(define (chainr1 p op)
    (define (scan) (Parser_and_then p [lambda (x) (rest_r x)]) )
    (define (rest_r x) (rest (scan) Parser_pure op x))
    
    (scan)
)

(define (between open close fp)
    (Parser_and_then
        (Parser_skip_get open fp)
        [lambda (x) (Parser_skip_get close [lambda () (Parser_pure x)])]
    )
)

(define (sqr x) (* x x))

(define (def_object n x)
    (Parser_skip (name n) (Parser_pure x))
)

(define (functions) (list
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

(define (func)
    (fold-left Parser_or_else (Parser_empty) (functions))
)

(define (m_E)        2.71828182845904523536)   ; e
(define (m_LOG2E)    1.44269504088896340736)   ; log2(e)
(define (m_LOG10E)   0.434294481903251827651)  ; log10(e)
(define (m_LN2)      0.693147180559945309417)  ; ln(2)
(define (m_LN10)     2.30258509299404568402)   ; ln(10)
(define (m_PI)       3.14159265358979323846)   ; pi
(define (m_PI_2)     1.57079632679489661923)   ; pi/2
(define (m_PI_4)     0.785398163397448309616)  ; pi/4
(define (m_1_PI)     0.318309886183790671538)  ; 1/pi
(define (m_2_PI)     0.636619772367581343076)  ; 2/pi
(define (m_2_SQRTPI) 1.12837916709551257390)   ; 2/sqrt(pi)
(define (m_SQRT2)    1.41421356237309504880)   ; sqrt(2)
(define (m_SQRT1_2)  0.707106781186547524401)  ; 1/sqrt(2)

(define (constants) (list
    (def_object "E"        (m_E))
    (def_object "LOG2E"    (m_LOG2E))
    (def_object "LOG10E"   (m_LOG10E))
    (def_object "LN2"      (m_LN2))
    (def_object "LN10"     (m_LN10))
    (def_object "PI"       (m_PI))
    (def_object "PI_2"     (m_PI_2))
    (def_object "PI_4"     (m_PI_4))
    (def_object "1_PI"     (m_1_PI))
    (def_object "2_PI"     (m_2_PI))
    (def_object "2_SQRTPI" (m_2_SQRTPI))
    (def_object "SQRT2"    (m_SQRT2))
    (def_object "SQRT1_2"  (m_SQRT1_2))
))

(define (const)
    (fold-left Parser_or_else (Parser_empty) (constants))
)

(define (op2 c f)
    (Parser_skip_get (symbol c) [lambda () (Parser_pure f)])
)

(define (add) (op2 #\+ +))
(define (sub) (op2 #\- -))
(define (mul) (op2 #\* *))
(define (div) (op2 #\/ /))
(define (pow) (op2 #\^ [lambda (x y) (exp (* y (log x)))]))

(define (expr)
    (chainl1 (term) (Parser_or_else_get (add) sub))
)

(define (term)
    (chainl1 (factor) (Parser_or_else_get (mul) div))
)

(define (factor)
    (chainr1 (factor0) (pow))
)

(define (expr_in_brackets)
    (define (br_open)  (symbol #\( ))
    (define (br_close) (symbol #\) ))
    
    (between (br_open) (br_close) expr)
)

(define (factor0)
    (Parser_or_else
        (expr_in_brackets)
        (Parser_apply (func) expr_in_brackets)
        (const)
        (double)
    )
)

(define (calculate s)
    (Parser_parse (expr) s)
)
