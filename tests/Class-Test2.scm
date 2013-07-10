(require racket/class)

(define test%
    (class object%
        (inspect #f)
        (init-field
            type
            address
        )
        (define/public (getAddress)
            address
        )
        (define/public (setAddress n)
            (set! address n)
        )
        (super-new)
    )
)

(define test2%
    (class test%
        (init-field
            parent
        )
        (define/public (setParent p)
            (set! parent p)
        )
        (define/public (getParent)
            parent
        )
    )
)

(define testy (new test% (type "cc") (address (vector 12 12))))
(define testo (new test2% (type "cc") (address (vector 12 12)) (parent "Korg")))

(define testH (make-hash))

(hash-set! testH testy (send testy getAddress))
(show "")
(show testH)

(show (hash-ref testH testo))

;(send testy setAddress 13)
;(show testH)
(show (hash-has-key? testH testo))