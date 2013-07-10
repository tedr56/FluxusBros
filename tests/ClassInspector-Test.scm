(require racket/class)

(define equal<%>
    (interface ()
        equal-to?
        equal-hash-code-of
        equal-secondary-hash-code-of
    )
)

(define test%
    (class object%
        (init-field
            type
            address
        )
        
        (define-values (player) #f)
        (define/public (getAddress)
            address
        )
        (define/public (setAddress n)
            (set! address n)
        )
        (define/public (setPlayer P)
            (set! player P)
        )
        (super-new)
    )
)

(define H (make-hash))

(define testy (new test% (type "cc") (address (vector 12 12))))
(send testy setPlayer "Korg")
(define testo (new test% (type "cc") (address (vector 12 12))))

(hash-set! H testy 12)

(show (hash-ref H testo))
