;shuriken
(require scheme/math)

(define (shuriken id cross)
    (let*
        (
            (rayon-c (c "rayon" id #:coeff 10))
            (g (c "gain" id #:coeff 5))
            (blur-c (+ 0.1 (c "blur" id)))
            (increment-max (c "increment-max" id))
            (size-cube (c "size-cube" id #:coeff 0.5))
            (scale-cube (c "scale-cube" id #:coeff 2))
            (scale-cube-gh (c "scale-cube-gh" id #:coeff 2))
            (nb-cube (/ (* 5 rayon-c pi) size-cube))
            (increment
                (lambda (i o f r)
                    (for ((n (build-list (inexact->exact (floor i)) values)))
                        (with-state
                            (colour (hsv->rgb (vector (/ (+ n 1) (+ i 1)) 1 (gh2 n g))))
            ;                (translate (vmul (vector (cos (atan (/ (* n o) r))) (sin (* n o)) 0) r))
                            (translate (vmul (vector (cos (/ (* n o 180) (* pi r))) (sin (/ (* n o 180) (* pi r))) 0) r))
                            (scale (vadd (vmul (vector 1 1 1) scale-cube) (vector 0 0 (gh2 i (* g scale-cube-gh)))))
            ;                (rotate (vector 0 0 (* (/ 360 (* (asin 1) 4)) (* n o))))
                            (draw-cube)
                        )
                    )
                )
            )
        )
        (push)
            (blur blur-c)
            (rotate (vector 180 0 -90))
            (increment (min (* (gh2 2 g) increment-max) nb-cube) size-cube 2 rayon-c)
        (pop)
    )

)
