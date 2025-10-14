; Maybe Support
(define (Just x) (list x))
(define Nothing '())

(define (IsJust? m)
    (! (length m) > 0)
)

; Functor
(define (Maybe_fmapr m f)
    (Maybe_and_then m [\\ (x) (Just (f x))])
)

; Monad
(define (Maybe_and_then m f)
    (if (IsJust? m) (f (car m)) Nothing)
)

; Alternative
(define  (Maybe_or_else_get m . fms)
    (fold-left
        [\\ (m1 fm) (if (IsJust? m1) m1 (fm))]
        m fms
    )
)
