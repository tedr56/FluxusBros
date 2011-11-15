(define number-points 100)
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
                            (hint-wire)
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
    (pdata-map!
        (lambda (p)
            (destroy (inexact->exact (vector-ref p 0)))
        )
        "prim"
    )
    (ungrab)
    (destroy (inexact->exact (hash-ref new-mew-prims id)))
    (hash-remove! new-mew-prims id)
    #t
)
(define (new-mew id cross)
    (letrec
        (
            (g (c "gain" id #:coeff 5))
            (neighbor-avoidance (c "neighbor-avoidance" id #:coeff 3))
            (home-attraction (c "home-attraction" id #:coeff 3))
            (speed (c "speed" id #:coeff 3))
            (acceleration (c "acceleration" id #:coeff 2))
            (colour-gain (c "colour-gain" id))
            (wire-colour-gain (c "wire-colour-gain" id))
            (wire-width (c "line-width-gh" id #:coeff 3))
            (flock
                (lambda (n p)
                    (unless (negative? n)
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
                            (ungrab)
                            (push)
                                (line-width (* (gh2 n g) wire-width))
                                (wire-colour (vmul (vector (gh2 n g) (gh2 (* n 2) g) (gh2 (* n 4) g)) wire-colour-gain))
                                (draw-line origin closest)
                            (pop)
                        )
                        (flock (- n 1) p)
                    )
                )
            )
        )
        (with-state
            (blur (c "blur" id))
            ;(line-width (c "line-width" id #:coeff 10))

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
                                (colour (hsv->rgb (vmul (vector (gh2 i g) (gh2 (* i 2) g) (gh2 (* i 4) g)) colour-gain)))
                                (wire-colour (hsv->rgb (vmul (vector (gh2 (* 2 i) g) (gh2 (* i 4) g) (gh2 (* i 2) g)) wire-colour-gain)))
                                (scale (vmul (vector (gh2 i g) (gh2 i g) (gh2 i g)) .051))
                            (ungrab)
                            (defil-z p #:v-init 10 #:v-min -30)
                        )
                        "p"
                    )
                (ungrab)
            )
        )
    )
)
