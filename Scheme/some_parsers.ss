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
    (token (! (++ alnum) >>= [\\ (s)
        (if (string=? s n) (Parser_pure n) Parser_empty)
    ]))
)

(define digits
    (** (satisfy char-numeric?))
)

(define sign
    (-- (! (char #\+) <||> (char #\-)))
)

(define double_
    (token (! (++ (satisfy char-numeric?)) >>=
        [\\ (s) (Parser_pure (string->number s))]
    ))
)

(define double
    (token (! sign >>=
        [\\ (sign_part) (! digits >>=
            [\\ (int_part) (!
                (--- (! (char #\.) >> digits)) >>=
                [\\ (frac_part) (!
                    (--- (!
                        (! (! (char #\e) <||> (char #\E)) >> sign) >>=
                        [\\ (exp_sign) (!
                            (++ (satisfy char-numeric?)) >>=
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
