(define break-sphere-prim (make-hash))
(define break-sphere-vel (make-hash))
(define break-sphere-rot-vel (make-hash))

(define (break-sphere-build id)
    
    (letrec
        (
            (prim
                (with-state
                    (hint-none)
                    (hint-points)
                    (hint-wire)
                    (scale 100)
                    
                    
                    
                    (build-sphere 20 20)
                    )
                )
            (round
                (lambda (d n (l '()))
                    (let*
                        (
                            (f-prim
                                (with-state
                                    
                                    (translate (vmul (hsrndvec) d))
                                    (parent prim)
                                    (recalc-normals 0)
                                    
                                    (build-sphere 3 3)
                                    )
                                )
                            (prim-list (append l (list f-prim)))
                            )
                        (with-primitive f-prim (recalc-normals 0))
                        (if (zero? n)
                            prim-list
                            (round d (- n 1) prim-list)
                            )
                        )
                    )
                )
            )
        (hash-set! break-sphere-prim id (append (list prim) (round 10 10) (round 20 20) (round 30 30)))
        (hash-set! break-sphere-vel  id (vmul (srndvec) 0.0000000000001))
        (hash-set! break-sphere-vel  id (vmul (srndvec) 1))
        (with-primitive prim
            (translate (vector -3 -3 -10))
            )
        )
    )

(define (break-sphere-destroy id)
    (for ((i (hash-ref break-sphere-prim id)))
        (destroy i)
        )
    (hash-remove! id)
    )

(define (break-sphere id cross)
    (when (beat-catch "break-sphere-test" "beat" #:beat (list 1))
        (with-primitive (car (hash-ref break-sphere-prim id))
            (hash-set! break-sphere-vel id (vmul (hash-ref break-sphere-vel id) -1))
            (hash-set! break-sphere-rot-vel id (vmul (srndvec) 3))
            )
        )
    #(when (beat-catch (send id get-name) #:beat (list 4))
        (hash-set! break-sphere-vel id (vadd (vmul (hash-ref break-sphere-vel id) -1) (vmul (srndvec) 0.5)))
        (hash-set! break-sphere-rot-vel id (vmul (srndvec) 5))
        )
    (with-primitive (car (hash-ref break-sphere-prim id))
        (translate (hash-ref break-sphere-vel id))
        (rotate    (hash-ref break-sphere-rot-vel id))
        )
    )
(require "beat-module.scm")
(require scheme/class)
(clear)
(break-sphere-build 1)
(every-frame (break-sphere 1 1))
