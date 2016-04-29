(define div 8)
(define profondeur-prims (make-hash))
(define (profondeur-build id)
    (when (hash-has-key? profondeur-prims id)
            (profondeur-destroy id)
    )
    (hash-set! profondeur-prims id  (build-ribbon (/ 128 div)))
    (with-primitive (hash-ref profondeur-prims id)
        (hint-unlit)
        (hint-vertcols)
        (translate (vector 9 5 -20))
        (rotate (vector 50 0 0))
        (pdata-map! (lambda (w) .1) "w")
    )
)
(define (profondeur-destroy id)
    (when (hash-has-key? profondeur-prims id)
        (destroy (hash-ref profondeur-prims id))
        (hash-remove! profondeur-prims id)
    )
)



;(define p2 (build-ribbon (/ 128 div)))
#(with-primitive p2
    (hide 1)
    (hint-unlit)
    (hint-vertcols)
    (rotate (vector 0 -90 0))
    (rotate (vector 90  0 0))
    (translate (vector 0 0 -3.0))
    (pdata-map! (lambda (w) .1) "w"))

(define (oscillo id cross)
    (let
        (
            [a (ga)]
            (gain (c "gain-oscillo" id #:coeff 100))
            (color (c "color-oscillo" id))
        )
        (with-primitive (hash-ref profondeur-prims id)
            (pdata-index-map!
                (lambda (i p)
                    ;(vector (* div .4 (- (* i 1) (/ (pdata-size) 2))) (* 10 (vector-ref a (* div i))) 0))
                    (vector
                        (* div .4 (- (* i 1) (/ (pdata-size) 2)))
                        ;1
                        (* gain (vector-ref a (* div i)))
                        0
                    )
                )
                "p"
            )
            (pdata-index-map!
                (lambda (i w)
                    (max 0 (* (vector-ref a (modulo (* div i) (get-num-frequency-bins))) 30 (c "oscillo-w" id)))
                    )
                "w")
            (pdata-index-map!
                (lambda (i c)
                    (vmul
                        (vmul
                            (hsv->rgb
                                (vector
                                    .7
                                    (- 1 (/ i (pdata-size)))
                                    (/ i (pdata-size))
                                )
                            )
                            (* (gh 1) .05 color)
                        )
                        1

                    )
                )
                "c"
            )
        )
        #(with-primitive p2
        (show (pdata-names))
            (pdata-index-map!
                (lambda (i p)
                    (vector (* div .25 (- (* i 1) (/ (pdata-size) 2))) (* 10 (vector-ref a (* div i))) 0))
                "p")
            (pdata-index-map!
                (lambda (i w)
                    (max 0 (min .1 (inexact->exact (round (* (vector-ref a (modulo (* div i) (get-num-frequency-bins))) 20)))))
                    )
                "w")
            (pdata-index-map!
                (lambda (i c)
                    (vmul (hsv->rgb (vector .7 (/ i (pdata-size)) (/ i (pdata-size)))) (mn 6 7))
                    )
                "c"
                )
            )
        
        ))

(define (caustik id cross n)

    (let*
        (
            (freq (random (get-num-frequency-bins)))
            (opa (min .9 (* 1 (gh freq))))
            )
        (with-state
            (hint-normalise)
            (translate (vector (random-center 30) 0 (random-center 30)))
            (translate (vmul (vector 0 0 (- (modulo-d (+ (* 2 freq) (time-now)) 30) 20)) 1))
            (rotate (vector 0 (time-now) 0))
            (rotate (vector 10 20 (+ -10 (random-center 5))))
            (scale (vector (min .4 (* .02 (gh freq))) 100 1))
            #(rotate (vector 60 30 0)) 
            (colour (vmul (hsv->rgb (vector .6 (* (c "color-caustik-gh-s" id #:coeff 1) 1 (gh freq)) (* .1 (c "color-caustik-gh-v" id #:coeff 10) (gh freq)) opa)) (c "color-caustik" id #:coeff 2)))
            (when (> opa 0.2)
                (draw-plane)
                )
            )
        (unless (zero? n)
            (caustik id cross (- n 1))
            )
        )
    )
(define (profondeur id cross)
    (random-seed 1)
    (oscillo id cross)
    (caustik id cross 200)
    )
;(every-frame (profondeur))
