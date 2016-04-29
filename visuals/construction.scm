(define construction-grid 10)

(define (draw-row id cross  x y z)
    (cond
        ((not (zero? x))
            (translate (vector 1 0 0))
            (with-state
                (let* ((vec (vtransform (vector 0 0 0) (get-transform)))
                   (dist (/ (vdist vec (vector 0 (* 1 (+ 1 (sin (* (c "center-speed" id #:coeff 5) (time-now))))) 0)) (max .0001 (* (c "center-size" id) (gl (modulo (+ x y z) (get-num-frequency-bins)))))))
                   (size (/ (max 0 (- 5 dist)) 5)))
                (scale
                    (vmul
                        (vector size size size)
                        (*
                            (c "scale" id #:coeff 3)
                            (+
                                1
                                (*
                                    (gh (modulo (+ x y z) 16))
                                    (c "scale-gh" id #:coeff .1)
                                )
                            )
                        )
                    )
                )
                )
                (if (< (inexact->exact (round (gh (modulo (+ x y z) (get-num-frequency-bins))))) (+ x y z))
                    (colour (hsv->rgb (vector 1 1 .6)))
                    (colour (vmul (vector 1 1 1) (min 1 (* .05 (gh (modulo (+ x y z) (get-num-frequency-bins)))))))
                )
                (draw-cube))
            (draw-row id cross (- x 1) y z))))


(define (draw-flat-grid id cross x y z)
    (cond
        ((not (zero? y))
            (translate (vector 0 1 0))
            (with-state (draw-row id cross x y z))
            (draw-flat-grid id cross x (- y 1) z))))

(define (draw-grid id cross x y z)
    (cond
        ((not (zero? z))
            (translate (vector 0 0 1))
            (with-state (draw-flat-grid id cross x y z))
            (draw-grid id cross x y (- z 1)))))

(define (cube-matrix id cross x y z)
    (with-state
    (rotate (vmul (vector (* .5 (time-now)) (* .8 (time-now)) (* 3 (time-now))) (c "rotate-speed" id #:coeff 20)))
    (translate (vector -5 -5 0))
    (draw-grid id cross x y z)
    )
)

(define (construction id cross)
    (cube-matrix id cross construction-grid construction-grid construction-grid)
)
