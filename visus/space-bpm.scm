(define (space-bpm-destroy id)
    (hash-for-each
        (hash-ref space-bpm-prims id)
        (lambda (p d)
            (destroy p)
        )
    )
    (hash-remove! space-bpm-prims id)
)
(define space-bpm-mode (make-hash))
(define space-bpm-prims (make-hash))

(define (space-bpm-add id)
    (let*
        (
            (new-cube (build-cube))
            (x-rnd 10)
            (y-rnd 8)
            (z-offset -20)
            (x (- (* x-rnd (flxrnd)) (* x-rnd 0.5)))
            (y (- (* y-rnd (flxrnd)) (* y-rnd 0.5)))
        )
        (with-primitive new-cube
            (translate (vector x y z-offset))
            (hash-set! (hash-ref space-bpm-prims id) new-cube (vector x y z-offset))
        )
    )
)
(define (space-bpm-prim-walk id prim z)
    (cond
        ((hash-has-key? space-bpm-prims id)
            (cond
                ((hash-has-key? (hash-ref space-bpm-prims id) prim)
                    (let*
                        (
                            (origine (hash-ref (hash-ref space-bpm-prims id) prim))
                            (destination (vadd origine (vector 0 0 z)))
                            (tour-operator  (string-append "space-bpm" "-" id "-" (number->string prim)))
                            (voyage (travel tour-operator origine destination 0.5))
                        )
                        (cond
                            ((equal? destination voyage)
                                (cond
                                    ((< (vector-ref destination 2) 10)
                                        (hash-set! (hash-ref space-bpm-prims id) prim destination)
                                        (travel-end tour-operator)
                                    )
                                    (else
                                        (destroy prim)
                                        (hash-remove! (hash-ref space-bpm-prims id) prim)
                                    )
                                )
                                #f
                            )
                            (else
                                (with-primitive prim
                                    (identity)
                                    (translate voyage)
                                    (colour (vmul (vector (gh prim) (gh (+ prim 2)) (gh (- prim 2))) 0.3))
                                    (scale (vmul (vector (gh prim) (gh (+ prim 2)) (gh (- prim 2))) 0.5))
                                )
                            )
                        )
                    )
                )
                (else
                    #f
                )
            )
        )
        (else
            #f
        )
    )
)
(define (space-bpm-walk id)
    (hash-for-each
        (hash-ref space-bpm-prims id)
        (lambda (p t)
            (spawn-task
                (lambda ()
                    (space-bpm-prim-walk
                        id
                        p
                        (gh p)
                    )
                )
                (string->symbol (string-append "space-bpm-walk-" (number->string p)))
            )
        )
    )
)

(define (space-bpm id cross)
    (unless (hash-has-key? space-bpm-mode id)
        (hash-set! space-bpm-mode id #t)
    )
    (unless (hash-has-key? space-bpm-prims id)
        (hash-set! space-bpm-prims id (make-hash))
    )
    (when (beat-catch id "space")
        (cond
            ((hash-ref space-bpm-mode id)
                (space-bpm-add id)
                (space-bpm-add id)
                (space-bpm-walk id)
                (hash-set! space-bpm-mode id #f)
            )
            (else
                (hash-set! space-bpm-mode id #t)
            )
        )
    )
)
            
