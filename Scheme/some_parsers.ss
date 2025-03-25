(load "parser.ss")

(define alnum
    (satisfy [\\ (c) (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_))])
)

(define (char c)
    (satisfy [\\ (x) (eq? x c)])
)

(define (symbol c)
    (token (char c))
)

(define (name n)
    (token (! (some alnum) >>= [\\ (s)
        (if (string=? s n) (Parser_pure n) Parser_empty)
    ]))
)

(define (optional_s p)
    (Parser_or_else_get p [\\ () empty_string])
)

(define (optional_c p)
    (optional_s
        (! [\\ (c) (string c)] <$> p)
    )
)

(define digits
    (many (satisfy char-numeric?))
)

(define sign
    (optional_c (! (char #\+) <||> (char #\-)))
)

(define double_
    (token (! (some (satisfy char-numeric?)) >>=
        [\\ (s) (Parser_pure (string->number s))]
    ))
)

(define double
    (token (! sign >>=
        [\\ (sign_part) (! digits >>=
            [\\ (int_part) (!
                (optional_s (! (char #\.) >> digits)) >>=
                [\\ (frac_part) (!
                    (optional_s (!
                        (! (! (char #\e) <||> (char #\E)) >> sign) >>=
                        [\\ (exp_sign) (!
                            (some (satisfy char-numeric?)) >>=
                            [\\ (exp_digits)
                                (Parser_pure (string-append exp_sign exp_digits))
                            ]
                        )]
                    )) >>=
                    [\\ (exp_part) (if
                        (or
                            (! (string-length int_part)  > 0)
                            (! (string-length frac_part) > 0)
                        )
                        (Parser_pure (string->number
                            (string-append
                                sign_part int_part
                                (if (! (string-length frac_part) > 0)
                                    (string-append "." frac_part)
                                    ""
                                )
                                (if (! (string-length exp_part) > 0)
                                    (string-append "e" exp_part)
                                    ""
                                )
                            )
                        ))
                        Parser_empty
                    )]
                )]
            )]
        )]
    ))
)
