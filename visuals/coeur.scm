(define (coeur id cross)
    (let
        (
            (freq (random (get-num-frequency-bins)))
            (color-gain (c "color-gain" id))
            (color-wire-gain (c "color-wire-gain" id))
            (trans-side (- (c "trans-side" id #:coeff 2) 1))
            (scale-gh (c "scale-gh" id #:coeff 5))
            (scale-random (c "scale-random" id))
            (speed-random (c "speed-random" id #:coeff 50))
            (speed (c "speed" id #:coeff 10))
        )

        (define (coeur-x id cross n)
            (with-state
                (hint-wire)
                (scale (vector .5 .5 .5))
                (scale (vmul (vector 1 1 1) (+ 1 (* scale-random (random 3)))))
                (translate (vector 0 -2 0))
                (translate (vmul (vector -20 0 0) trans-side))
                (translate (vmul (vector 2 0 0) (* n trans-side)))
                (scale (vector 2 (max -0.1 (* (gl freq) scale-gh))  6))
                (wire-colour (vmul (hsv->rgb (vector (+ (/ (random 100) 1000) 0) 1 1)) color-wire-gain))
                (colour (vmul (hsv->rgb (vector 1 0 1)) color-gain))
                (opacity color-gain)
                (draw-cube)
            )
            (unless (<= n 0)
                (coeur-x id cross (- n 1))
            )
        )

    (define (coeur-z id cross n m)
        (random-seed (max (inexact->exact (round m)) 1))
        (with-state
            (translate (vector 0 -1 -100))
            (translate (vector 0 0 (* 15 (modulo-d (* speed (+ m speed-random .02 (time-now))) 10))))
            (coeur-x id cross n)
            )
        (unless (<= m 0)
            (coeur-z id cross n (- m 1))
        )
    )


    (coeur-z id cross (c "cubes-x" id #:coeff 20) (c "cubes-y" id #:coeff 20))
    #t
    
  )
)
(show "coeur loaded")
