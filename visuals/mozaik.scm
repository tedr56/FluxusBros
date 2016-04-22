(define (mozaik id cross)
    (with-state
        (letrec
            (
                (g (c "gain" id))
                (blur-c (c "blur" id))
                (limit-x (c "limit-x" id #:coeff 10))
                (limit-y (c "limit-y" id #:coeff 10))
                (fact-time-x-coeff (c "fact-time-x" id #:coeff 127))
                (fact-time-y-coeff (c "fact-time-y" id #:coeff 127))
                (fact-time-x-sens (c "fact-time-x-sens" id #:coeff 127))
                (fact-time-y-sens (c "fact-time-y-sens" id #:coeff 127))
                (threshold-x (c "threshold-x" id #:coeff 127))
                (threshold-y (c "threshold-y" id #:coeff 127))
                (ope1 (c "ope1" id #:coeff 127))
                (ope2 (c "ope2" id #:coeff 127))
                (wave-speed (c "wave-speed" id #:coeff 127))
                (decal (c "decal" id))
                (amp (c "amp" id #:coeff 10))
                (globloc-x-select (c "globloc-x" id #:coeff 127))
;                (globloc-y-select (c "globloc-y" id #:coeff 127))
                (m4033 (c "m4033" id #:coeff 4))
                (axe-sens (c "axe-sens" id #:coeff 127))
                (rot-speed (c "rot-speed" id #:coeff (* 127 3)))
                (rot-sens (c "rot-sens" id #:coeff 127))
                (factor-gh (c "factor-gh" id #:coeff 0.01))
                (factor (c "factor" id))
                (opacity-c (c "opacity" id))
                (fact-color-c (c "fact-color" id))
                (color-select (c "color-select" id #:coeff 127))
                (quarter-select-x (c "quarter-select-x" id #:coeff 127))
                (quarter-select-y (c "quarter-select-y" id #:coeff 127))
                (ope
                    (lambda (input)
                        (cond
                            ((< input (/ 127 3))
                                +)
                            ((< input (* (/ 127 3) 2))
                                -)
                            (else *))))
                (draw-cube-p
                    (lambda (x y)
                        (let*
                            (
                                (fact-time-x (* fact-time-x-coeff (if (< fact-time-x-sens 63) 1 -1)))
                                (fact-time-y (* fact-time-y-coeff (if (< fact-time-y-sens 63) 1 -1)))
                            )
                            (when
                                (and
                                    (cond ((< threshold-x (/ 127 3))
                                                (> (gh2 (+ x (* fact-time-x (time-now))) g) limit-x))
                                          ((< threshold-x (* (/ 127 3) 2))
                                            (and
                                                (> (gh2 (abs ((ope ope1) (+ x (* fact-time-x (time-now))) (+ y (* fact-time-y (time-now))))) g) limit-x)
                                                (> (gh2 (abs ((ope ope2) (+ x (* fact-time-x (time-now))) (+ y (* fact-time-y (time-now))))) g) limit-y))))
                                    (cond ((< threshold-y (/ 127 3))
                                        (> (gh2 (+ y (* fact-time-y (time-now))) g) limit-y)
                                    ))
                                )
                                (draw-cube)))))
                (wave
                    (lambda (X Y signX signY)
                        (let*
                            (
                                (x (* X signX))
                                (y (* Y signY))
                                (speed (* 0.2 wave-speed (time-now)))
                                (globloc-x (if (< globloc-x-select 63) x X))
                                (globloc-y globloc-x)
;                                (globloc-y (if (< (m chann 24) 63) y Y))
                                (axe
                                    (cond
                                        ((< m4033 1)
                                            globloc-x)
                                        ((< m4033 2)
                                            globloc-y)
                                        ((< m4033 3)
                                            (+ globloc-y globloc-x))
                                        ((<= m4033 4)
                                            (* globloc-y globloc-x))))
                                (axe-sign (if (< axe-sens 63) axe (* -1 axe)))
                            )
                            (when (> amp 0)
                                (translate (vector 0 0 (* amp (sin (+ (* axe-sign decal) speed)))))
                                (scale (vector 1 1 (abs (* amp (sin (+ (* axe-sign decal) speed))))))))))
                (rotate-p
                    (lambda (x y)
                        (let*
                            (
                                (speed (* rot-speed (time-now)))
                            )
                            (when (or (and (< rot-sens (/ 127 3)) (odd? y)) (and (> rot-sens (* (/ 127 3) 2)) (odd? y)))
                                (rotate (vector 0  speed 0)))
                            (when (and (> rot-sens (/ 127 3) ) (even? y))
                                (rotate (vector 0  (* -1 speed) 0))))))
                (distance
                    (lambda (X Y)
                        (let*
                            (
                                (dist (vdist (vector 0 0 0) (vector X Y 0)))
                                (fact (+ (* (gh2 2 g) factor-gh) factor))
                            )
                            (scale (vector 1 1 (+ 1 (* (* fact fact) dist)))))))
                (quarter
                    (lambda (X Y signX signY)
                        (let
                            (
                                (x (* X signX))
                                (y (* Y signY))
                            )
                            (push)
                                (opacity opacity-c)
                                (rotate-p X Y)
                                (translate (vector x y 0))
                                (wave X Y signX signY)
                                (distance X Y)
                                (draw-cube-p X Y)
                            (pop))))
                (draw-row
                    (lambda (x y maxX maxY)
                        (let
                            (
                                (-x (* -1 x))
                                (-y (* -1 y))
                                (Nb-colors 4)
                                (fact-color (+ 0.01 fact-color-c))
                            )
                            (cond ((< color-select (/ 127 Nb-colors))
                                        (colour (vmul (vector (gh2 x g) (gh2 y g) (* 0.5 (gh2 (+ x y) g))) fact-color)))
                                  ((< color-select (* (/ 127 Nb-colors) 2))
                                        (colour (vmul (vector (* 0.5 (gh2 (+ x y) g)) (gh2 x g) (gh2 y g)) fact-color)))
                                  ((< color-select (* (/ 127 Nb-colors) 3))
                                        (colour (vmul (vector (gh2 y g) (* 0.5 (gh2 (+ x y) g)) (gh2 x) g) fact-color)))
                                  ((< color-select (* (/ 127 Nb-colors) 4))
                                        (colour (vmul (vector (gh2 2 g) (* (gh2 (* x y) g) 1) (gh2 (* x y) g)) fact-color)))
                            )
                            (cond ((< quarter-select-x (/ 127 3))
                                    (quarter x y 1 1))
                                  ((< quarter-select-x (* 2 (/ 127 3)))
                                    (quarter x y -1 1))
                                  (else
                                    (quarter x y 1 1)
                                    (quarter x y -1 1)))
                            (cond ((< quarter-select-y (/ 127 3))
                                    (quarter x y 1 -1))
                                  ((< quarter-select-y (* 2 (/ 127 3)))
                                    (quarter x y -1 -1))
                                  (else
                                    (quarter x y 1 -1)
                                    (quarter x y -1 -1)))

                            (when (< x maxX)
                                (draw-row (+ 1 x) y maxX maxY))
                        )
                    )
                )
                (draw-rows
                    (lambda (x y maxX maxY)
                        (draw-row x y maxX maxY)
                        (when (< y maxY)
                            (draw-rows x (+ 1 y) maxX maxY))
                    )
                )
            )
            (shinyness 20)
            (persp)
            (blur blur-c)
            (draw-rows 0 0 (* 2 16) (* 1.3 16))
        )
    )
)

