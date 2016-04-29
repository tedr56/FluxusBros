(define s0 (with-state (hide 1)(scale 0)(build-sphere 2 2)))

(define (radiohazard id cross)

    (letrec
        (
            (gain-c (c "gain" id))
            (coeffPrims (* (c "coeffPrims" id) 1000))
            (speed (* (c "speed" id) 30))
            (Xcoeff (* (c "Xcoeff" id) 30))
            (Ycoeff (* (c "Ycoeff" id) 20))
            (coeffRadius (+ 0.0001 (* (c "coeffRadius" id) 10)))
            (sizeRadius (* (c "sizeRadius" id) 10))
            (scaleGh (* (c "scaleGh" id) 10))
            (coeffDistrodX (c "coeffDistrodX" id))
            (coeffDistrodY (c "coeffDistrodY" id))
            (colourRadius (c "colourRadius" id))
            (colourBase (c "colourBase" id))
            (rotZSpeed (* (c "rotZSpeed" id) 100))
            (blur-c (c "blur" id))
            (shiny (* (c "shiny" id) 100))
            (CenterCoeff
                (lambda (coeff (value (flxrnd)))
                    (- (* value coeff) (/ coeff 2))
                )
            )
            (cube-travel-space
                (lambda (offset Zcoeff n)

                    (with-state
                        (let*
                            (
                                (pos (vector (CenterCoeff Xcoeff) (CenterCoeff Ycoeff) (- (fmod (+ (* (flxrnd) Zcoeff) offset (* speed (time-now))) 40) 40)))
                                (glDistord (* 5 (- (max 0 (gl n)) (max 0 (gl (+ n 1))))))
                                (dist (vdist-sq (vector (* coeffDistrodX glDistord) (* coeffDistrodY glDistord) -10) pos))
                                (coeffdist (max 0 (+ coeffRadius (* (atan (/  (* (max 0.000001 sizeRadius)  (max 0.000001 coeffRadius) -1))) dist 1))))
                            )
                            (rotate (vector 0 0 (* rotZSpeed (- (flxrnd) .5) (time-now))))
                            (colour (hsv->rgb (vector (- colourBase (* colourRadius coeffdist)) 0.2 1)))
                            (opacity (min cross (gl n gain-c)))
                            (shinyness shiny)
                            (translate pos)
                            (scale (vmul (vector .1 .1 (clamp glDistord .1 1)) coeffdist))
                            (draw-instance s0)
                        )
                    )
                )
            )
            (cubes-travels
                (lambda (n)
                    (when (positive? n)

                        (cube-travel-space (* 1000 (flxrnd)) (* 5 (flxrnd)) n)
                        (cubes-travels (- n 1))
                    )
                )
            )
        )
        (flxseed 1)
        (with-state
            (blur blur-c)
            (cubes-travels coeffPrims)
        )
    )
)
