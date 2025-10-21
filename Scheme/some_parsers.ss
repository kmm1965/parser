(load "parser.ss")

(define alnum
    (satisfy [\\ (c) (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_))])
)

(define (char c) (satisfy [\\ (x) (eq? x c)]))

(define (symbol c) (token (char c)))

(define (name n)
    (token (do!
        (s <- (++ alnum))
        (if (string=? s n) (Parser_pure n) Parser_empty)
    ))
)

(define digit (satisfy char-numeric?))

(define digits (** digit))

(define sign (-- (! (char #\+) <||> (char #\-))))

; Unary sign
(define usign (token sign))

(define double
    (token (do!
        (int_part  <- digits)
        (frac_part <- (--- (! (char #\.) >>> digits)))
        (exp_part  <- (--- (do!
            (exp_sign   <- (! (! (char #\e) <||> (char #\E)) >>> sign))
            (exp_digits <- (++ digit))
            (Parser_pure (string-append exp_sign exp_digits))
        )))
        (if (or (not (string-empty? int_part)) (not (string-empty? frac_part)))
            (Parser_pure (string->number (string-append int_part
                (if (not (string-empty? frac_part)) (string-append "." frac_part) "")
                (if (not (string-empty? exp_part))  (string-append "e" exp_part) "")
            )))
            Parser_empty
        )
    ))
)
