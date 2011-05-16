(define spectral-wall-hash (make-hash))

(define (spectral-wall-build id)
;    (hash-set! spectral-wall-hash id (build-seg-plane (get-num-frequency-bins)))
    (hash-set! spectral-wall-hash id (build-seg-plane (vector-length (ga)) 10))
)

(define (spectral-wall id cross)
    (unless (hash-has-key? spectral-wall-hash id)
        (spectral-wall-build id)
    )
    (let
        (
            (new-range
                (list->vector
                    (map
                        (lambda (v)
                            (gl v)
                        )
                        (build-list (get-num-frequency-bins) values)
                    )
                )
            )
        )
        (letrec
            (
                (get-previous-range-index
                    (lambda (i)
                        (- i (* 4 (vector-length new-range)))
                    )
                )
                (add-range
                    (lambda (r)
                        (with-primitive (hash-ref spectral-wall-hash id)
                            (pdata-index-map!
                                (lambda (i p)
                                    (cond
                                        ((< i (- (- (pdata-size) 1) (vector-length new-range)))
                                            (pdata-ref "p" (get-previous-range-index i))
                                        )
                                        (else
                                            (vector-ref new-range (inexact->exact (floor (/ i 4))))
                                        )
                                    )
                                )
                                "p"
                            )
                        )
                    )
                )
            )
            (add-range new-range)
        )
    )
)


;(every-frame (spectral-wall 1 1)) 