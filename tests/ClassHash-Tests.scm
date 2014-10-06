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

(define testy (new test% (type "cc") (address (vector 12 12))))
(define testo (new test% (type "cc") (address (vector 12 12))))

(define testa (new test% (type "cc1") (address 13)))
(define testb (new test% (type "cc1") (address 13)))

(define testH (make-hash))

(hash-set! testH testy (send testy getAddress))
(hash-set! testH testa (send testa getAddress))
(show "")
(show testH)

(show (hash-ref testH testo))

;(send testy setAddress 13)
;(show testH)
(show (hash-has-key? testH testo))
(show (hash-has-key? testH testb))
;(show (hash-iterate-key testH (hash-iterate-first testH)))
;(show testy)
;(show (equal? testy (hash-iterate-key testH (hash-iterate-first testH))))