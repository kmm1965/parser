; String support
(define (string-head s)
    (car (string->list s))
)

(define (string-tail s)
    (substring s 1 (string-length s))
)

(define (string-empty? s)
    (zero? (string-length s))
)

(define-syntax !
    (syntax-rules (<$> <*> >>= >> >>> <||>)
        ((_ a) a)
        ((_ a <$> b) (<$> a b))
        ((_ a <*> b) (<*> a b))
        ((_ a >>= b) (>>= a b))
        ((_ a >> b) (>> a b))
        ((_ a >>> b) (>>> a b))
        ((_ a <||> b) (<||> a b))
        ((_ a > b) (> a b))
    )
  )

(define-syntax \\
    (syntax-rules ()
        ([_ params body] [lambda params body])
    )
)

(define-syntax **
    (syntax-rules ()
        ((_ p) (many p))
    )
)

(define-syntax ++
    (syntax-rules ()
        ((_ p) (some p))
    )
)

(define-syntax --
    (syntax-rules ()
        ((_ p) (optional_c p))
    )
)

(define-syntax ---
    (syntax-rules ()
        ((_ p) (optional_s p))
    )
)

; do notation
(define-syntax do!
  (syntax-rules (<-)
    [(_ expr) expr] ; End of recursion

    [(_ (var <- expr) more ...) ; Base case with arrows
      (! expr >>= [\\ (var) (do! more ...)])
    ]

    [(_ expr more ...) ; Case without arrows
      (! expr >> [\\ () (do! more ...)])
    ]
  )
)
