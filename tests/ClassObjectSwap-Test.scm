(require racket/class)
(define T (make-hash))
(define control%
    (class object%
        (init-field
            value
        )
        (define/public (getValue) value)
        (define/public (setValue key val) (hash-set! value key val))
        (super-new)
    )
)

(define a (new control% (value T)))
(define b (new control% (value T)))

(show "")(show "")(show "")
(hash-set! T 123 123)
(show (send a getValue))
(show (send b getValue))

(send a setValue 234 234)
(send b setValue 345 345)

(show (send a getValue))
(show (send b getValue))
 