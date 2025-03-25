; String support
(define (string-head s)
    (car (string->list s))
)

(define (string-tail s)
    (substring s 1 (string-length s))
)

(define-syntax !
    ;(syntax-rules (<$> <*> >>= >> <||> >)
    (syntax-rules (<$> <*> >>= >> <||> >)
        ((_ a) a)
        ((_ a <$> b) (<$> a b))
        ((_ a <*> b) (<*> a b))
        ((_ a >>= b) (>>= a b))
        ((_ a >> b) (>> a b))
        ((_ a <||> b) (<||> a b))
        ((_ a > b) (> a b))
    )
  )

(define-syntax \\
    (syntax-rules ()
        ([_ params body] [lambda params body])
    )
)
