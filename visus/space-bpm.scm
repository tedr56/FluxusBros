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


(define (space-bpm id cross)
    (unless (hash-has-key? space-bpm-mode id)
        (hash-set! space-bpm-mode id #t)
    )
    (letrec
        (
            (Name (send id get-name))
            (g (c "gain" id))
            (control-scale (c "scale" id  #:coeff 3))
            (control-colour (c "colour" id))
            (control-beat-add (c "space-beat-add" id #:coeff 15))
            (control-beat (c "space-beat" id))
            (control-beat-offset (c "space-beat-offset" id #:coeff 10))
            (blur2 (c "blur" id))
            (space-bpm-add
                (lambda ()
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
            )
            (space-bpm-prim-walk
                (lambda (prim z)
                    (cond
                        ((hash-has-key? space-bpm-prims id)
                            (cond
                                ((hash-has-key? (hash-ref space-bpm-prims id) prim)
                                    (let*
                                        (
                                            (origine (hash-ref (hash-ref space-bpm-prims id) prim))
                                            (destination (vadd origine (vector 0 0 z)))
                                            (tour-operator  (string-append "space-bpm" "-" (send id get-name) "-" (number->string prim)))
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
                                                    (colour (vmul (vector (gl prim g) (gl (+ prim 2) g) (gl (- prim 2) g)) control-colour))
                                                    (scale (vmul (vector (gl prim g) (gl (+ prim 2) g) (gl (- prim 2) g)) control-scale))
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
            )
            (space-bpm-walk
                (lambda()
                    (hash-for-each
                        (hash-ref space-bpm-prims id)
                        (lambda (p t)
                            (spawn-task
                                (lambda ()
                                    (space-bpm-prim-walk
                                        p
                                        (* (gl p g) control-beat-offset)
                                    )
                                )
                            (string->symbol (string-append "space-bpm-walk-" (number->string p)))
                            )
                        )
                    )
                )
            )
        )
        (unless (hash-has-key? space-bpm-prims id)
            (hash-set! space-bpm-prims id (make-hash))
        )
        (when (beat-catch Name "space" #:beat (list control-beat))
            (cond
                ((hash-ref space-bpm-mode id)
                    (for ((i (build-list (inexact->exact (round control-beat-add)) values)))
                        (space-bpm-add)
                    )
                    (space-bpm-walk)
                    (hash-set! space-bpm-mode id #f)
                )
                (else
                    (hash-set! space-bpm-mode id #t)
                )
            )
        )
    )
)
