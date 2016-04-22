(require scheme/math)

(define (degre->pi a)
    (* a (/ (* 2 pi) 360))
)

(define (spherique->cartesien v)
    (let
        (
            (p (vector-ref v 2))
            (g (vector-ref v 1))
            (d (vector-ref v 0))
        )
        (vector
            (* p (cos (degre->pi g)) (sin (degre->pi d)))
            (* p (sin (degre->pi g)) (cos (degre->pi d)))
            (* p (cos (degre->pi g)) (cos (degre->pi d)))
        )
    )
)

(define (square-sphere id cross)
    (let*
        (
            (g 1)
            (speed-G 100)
            (speed-L 00)
            (tall 5)
            (nb-element 10)
            (sphere-size (/ (* nb-element tall) (* 2 pi)))
            (-sphere-size (* -1 sphere-size))
        )
        (letrec
            (
                (square-group
                    (lambda (X Y Z size color freq n n-max)
                        (square X Y (+ Z (* (/ n n-max) n -0.05)) size color freq)
                        (when (< n n-max)
                            (square-group X Y Z size color freq (+ n 1) n-max)
                        )
                    )
                )
                (square
                    (lambda (X Y Z size color freq)
                        (with-state
;                            (hint-none)
                            (hint-wire)
                            (wire-colour color)
                            (wire-opacity (* (gl freq) 0.7))
                            (rotate (vector 0 0 (* speed-G (time-now))))
                            (scale size)
                            (translate (spherique->cartesien (vmul2 (vector X Y Z) (vector sphere-size sphere-size -sphere-size))))
(translate (vector 0 0 (* 1 (* 1.2 tall))))
;                            (rotate (vmul2 (vector X Y Z) (vector sphere-size sphere-size -sphere-size)))
;                            (scale (min (max 0.5 (* (gl freq) 0.6)) 3))
                            (rotate (vector 0 0 (* -1 speed-G (time-now))))
                            (rotate (vector 0 0 (* -1 speed-L (time-now))))
                            (draw-plane)
                        )
                    )
                )
            )

            (set-gain! g)
            (line-width 3)

            (square-group 0 0 1 tall (vector 0.5 0.5 0.9) 2 0 (gl 2))

            (square-group 1 0 1 tall (vector 0.2 0.7 0.2) 5 0 (gl 5))
            (square-group -1 0 1 tall (vector 0.2 0.7 0.2) 5 0 (gl 5))
            (square-group 0 1 1 tall (vector 0.7 0.7 0.9) 8 0 (gl 8))
            (square-group 0 -1 1 tall (vector 0.7 0.7 0.9) 8 0 (gl 8))

            (square-group 1 1 1 tall (vector 0.7 0 0) 12 0 (gl 12))
            (square-group 1 -1 1 tall (vector 0.7 0 0) 12 0 (gl 12))
            (square-group -1 1 1 tall (vector 0.7 0 0) 15 0 (gl 15))
            (square-group -1 -1 1 tall (vector 0.7 0 0) 15 0 (gl 15))

        )
    )
)
