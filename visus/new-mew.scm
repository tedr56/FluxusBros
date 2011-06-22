(define (defil-z v #:v-init (v-init 0) #:v-max (v-max 10) #:v-min (v-min -30))
    (cond
        ((>= (vector-ref v 2) v-max)
            (vector (vector-ref v 0) (vector-ref v 1) v-init)
        )
        ((<= (vector-ref v 2) v-min)
            (vector (vector-ref v 0) (vector-ref v 1) v-init)
        )
    )
)
(define new-mew-prims (make-hash))
(define (new-mew-build id (nb 10))
    (let
        (
            (particles (build-particles nb))
        )
        (grab particles)
            (hint-none)
            (pdata-map!
                (lambda (v)
                    (vector
                        (* (- (* (flxrnd) 2) 1) 20)
                        (* (- (* (flxrnd) 2) 1) 16)
                        (* (- (* (flxrnd) 2) 1) 16)
                    )
                )
                "p"
            )
            (pdata-add "prim" "v")
            (pdata-add "vel" "v")
            (pdata-map!
                (lambda (v)
                    (let
                        (
                            (p (build-cube))
                        )
                        (grab p)
                            (identity)
                            (translate v)
                        (ungrab)
                        (vector p 0 0)
                    )
                )
                "prim"
            )
        (ungrab)
        (hash-set! new-mew-prims id particles)
    )
)
(define (new-mew-destroy id)
    (grab (hash-ref new-mew-prims id))
    (pdata-map
        (lambda (p)
            (destroy (vector-ref p 0))
        )
        "prim"
    )
    (ungrab)
    (destroy (hash-ref new-mew-prims id))
    (hash-remove! new-mew-prims id)
)
(define (new-mew id cross)
    (letrec
        (
            (g 1)
            (neighbor-avoidance 2.0)
            (home-attraction 2)
            (speed 1)
            (acceleration 1)
            (flock
                (lambda (n p)
                    (unless (zero? n)
                        (let
                            (
                                (closest 0)
                                (origin 0)
                            )
                            (grab p)
                            (set! closest (pdata-op "closest" "p" n))
                            (set! origin (pdata-ref "p" n))
                            (let*
                                (
                                    (dir (vmul (vnormalise (vsub (pdata-get "p" n) closest)) neighbor-avoidance))
                                    (centre (vmul (vnormalise (vsub (vector 0 0 -40) (pdata-get "p" n))) home-attraction))
                                    (velocity 
                                        (vmul
                                            (vnormalise
                                                (vadd (vmul (vadd dir centre) (* (gh2 n g) speed)) (pdata-get "vel" n)))
                                            acceleration
                                        )
                                    )
                                )
                                (pdata-set "vel" n velocity)
                                )
                            )
                            (ungrab)
                            (push)
                                (line-width (* (gh2 n g) .01))
                                (wire-colour (vmul (vector (gh2 n g) (gh2 (* n 2) g) (gh2 (* n 4) g)) .02))
                                (draw-line origin closest)
                            (pop)
                        )
                        (flock (- n 1) p)
                    )
                )
            )
        )
        (unless (hash-has-key? new-mew-prims id)
            (new-mew-build id number-points)
        )
        (let
            (
                (prim (hash-ref new-mew-prims id))
            )
            (flock number-points prim)
            (grab prim)
                (pdata-op "+" "p" "vel")
                (pdata-index-map!
                    (lambda (i p)
                        (grab (inexact->exact (vector-ref (pdata-ref "prim" i) 0)))
                            (identity)
                            (translate p)
                            (opacity (* (gh2 i) .01))
                            (colour (hsv->rgb (vmul (vector (gh2 i g) (gh2 (* i 2) g) (gh2 (* i 4) g)) .02)))
                            (scale (vmul (vector (gh2 i g) (gh2 (* i 2) g) (gh2 (* i 4) g)) .051))
                        (ungrab)
                        (defil-z p #:v-init 10 #:v-min -30)
                    )
                    "p"
                )
            (ungrab)
        )
    )
)
