(load "utils.ss")
(load "maybe.ss")

(define (parse p inp)
    (p inp)
)

; Functor
(define (<$> f p)
    (! p >>= [\\ (x) (Parser_pure (f x))])
)

; Monad
(define (>>= p0 . fs)
    (fold-left
        [\\ (p f)
            [\\ (inp)
                (Maybe_and_then (parse p inp)
                    [\\ (pair) (parse (f (car pair)) (cadr pair))]
                )
            ]
        ]
        p0 fs
    )
)

(define (>> p0 . qs)
    (fold-left
        [\\ (p q) (! p >>= [\\ (_) q])]
        p0 qs
    )
)

; Applicative
(define (Parser_pure x)
    [\\ (inp) (Just (list x inp))]
)

(define (<*> pf0 . fqs)
    (fold-left
        [\\ (pf fq) (! pf >>= [\\ (f) (! f <$> (fq))])]
        pf0 fqs
    )
)

(define (liftA2 f)
    [\\ (p1 p2) (!(! f <$> p1) <*> p2)]
)

; Alternative
(define Parser_empty
    [\\ (_) Nothing]
)

(define (<||> . ps)
    [\\ (inp)
        (fold-left
            [\\ (m p) (Maybe_or_else_get m [\\ () (parse p inp)])]
            Nothing ps
        )
    ]
)

(define (Parser_or_else_get p . fps)
    [\\ (inp)
        (fold-left
            [\\ (m fp) (Maybe_or_else_get m [\\ () (parse (fp) inp)])]
            (parse p inp) fps
        )
    ]
)

(define anyChar
    [\\ (inp)
        (if (! (string-length inp) > 0)
            (Just (list (string-head inp) (string-tail inp)))
            Nothing
        )
    ]
)

(define (satisfy pred?)
    (! anyChar >>= [\\ (c)
        (if (pred? c)
            (Parser_pure c)
            Parser_empty
        )]
    )
)

(define empty_string
    (Parser_pure "")
)

(define (optional_s p)
    (Parser_or_else_get p [\\ () empty_string])
)

(define (optional_c p)
    (optional_s (! [\\ (c) (string c)] <$> p))
)

(define (some p)
    ((liftA2 [\\ (c) [\\ (s) (string-append (string c) s)]])
        p [\\ () (** p)]
    )
)

(define (many p)
    (--- (++ p))
)

(define spaces
    (** (satisfy char-whitespace?))
)

(define (between open close fp)
    (!
        (! open >>= [\\ (_) (fp)])
        >>= [\\ (x) (! close >> (Parser_pure x))]
    )
)

(define (token p)
    (between spaces spaces (\\ () p))
)

(define (rest p ff op x)
    (Parser_or_else_get
        (! op >>=
            [\\ (f) (! p >>=
                [\\ (y) (ff (f x y))]
            )]
        )
        [\\ () (Parser_pure x)]
    )
)

(define (chainl1 p op)
    (define (rest_l x) (rest p rest_l op x))

    (! p >>= [\\ (x) (rest_l x)])
)

(define (chainr1 p op)
    (define scan (! p >>= [\\ (x) (rest_r x)]) )
    (define (rest_r x) (rest scan Parser_pure op x))
    
    scan
)
