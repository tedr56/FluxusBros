;foret


(define (foret id cross)
    (with-state
        (let
            (
                (Name (symbol->string (send id get-name)))
                (g (c "gain" id #:coeff 0.5))
            )
            (letrec
                (
                    (ligne
                        (lambda (x y maxX)
                            (let
                                (
                                    (color
                                        (vector 
                                            (* (* 10.1 (* (c "color-r" id) (gh2 2 g))) (+ (flxrnd) 0.01)) 
                                            (* (* 0.3 (* (c "color-g" id) (gh2 8 g))) (+ (flxrnd) 0.01)) 
                                            (* (* 0.5 (* (c "color-b" id) (gh2 15 g))) (+ (flxrnd) 0.01)))) 
                                    (wire-color
                                        (vector
                                            (* (* 5.1 (* (c "wire-color-r" id) (gh2 2 g))) (+ 1 1)) 
                                            (* (* 0.05 (* (c "wire-color-g" id) (gh2 8 g))) (+ 1 1)) 
                                            (* (* 0.5 (* (c "wire-color-b" id) (gh2 15 g))) (+ 1 1)))) 
                                    (line-ep (+ 0.1 (* 10 (c "line-width" id))))
                                )
                                (cond
                                    ((= 1 (c "wire" id))
                                        (hint-none)(hint-wire))
                                    ((= 1 (c "wire-stippled" id))
                                        (hint-none)(hint-wire-stippled))
                                    (else
                                        (hint-none)(hint-solid)
                                    )
                                )

                                (push)
                                    (translate (vector (* 4 x) y 0))
                                    (colour color)
                                    (wire-colour wire-color)
                                    (line-width line-ep)
                                    (let
                                        (
                                            (aleatoire (gh2 (* (flxrnd)  12 g)))
                                        )
                                        (translate (vector 0 0 (* -0.5 aleatoire)))
                                        (scale (vector 1 1 aleatoire) )

                                    )    
                                    (draw-cube)
                                (pop)
                                (push)
                                    (translate (vector (* -4 x) y 0))
                                    (colour color)
                                    (wire-colour wire-color)
                                    (line-width line-ep)
                                    (let
                                        (
                                            (aleatoire (gh2 (* (flxrnd)  12 g)))
                                        )
                                        (translate (vector 0 0 (* -0.5 aleatoire)))
                                        (scale (vector 1 1 aleatoire) )

                                    ) 
                                    (draw-cube)
                                (pop)
                            )
                            (when (< x maxX)
                                (ligne (+ x 1) y maxX)
                            )
                        )
                    )
                    (ligne+
                        (lambda (x y maxX maxY)
                            (push)
                                (translate (vector 0 (* 4 y) 0))
                                (ligne x y maxX)
                            (pop)
                            (push)
                                (translate (vector 0 (* -4 y) 0))
                                (ligne x y maxX)
                            (pop)
                            (when (< y maxY)
                                 (ligne+ x (+ y 1) maxX maxY)
                            )
                        )
                    )
                )
                (flxseed 2)
                (let*
                    (
                        (coeffx 200)
                        (coeffy 100)
                        (coeffz 300)
                        (tx (c "tx" id #:coeff coeffx))
                        (ty (c "ty" id #:coeff coeffy))
                        (tz (c "ty" id #:coeff coeffz))
                        (Tx (- tx (* 0.5 coeffx)))
                        (Ty (- ty (* 0.5 coeffy)))
                        (Tz (- tz (* 0.5 coeffz)))
                    )
                    (translate (vector Tx Ty Tz))
                )
                (let
                    (
                        (sensx (- (* 2 (c "sens-x-1" id)) 1))
                        (sensy (- (* 2 (c "sens-y-1" id)) 1))
                        (sensz (- (* 2 (c "sens-z-1" id)) 1))
                    )


                    (tempo (string-append "rtx" Name) (* sensx (c "rtx" id #:coeff 100)))
                    (tempo (string-append "rty" Name) (* sensy (c "rty" id #:coeff 100)))
                    (tempo (string-append "rtz" Name) (* sensz (c "rtz" id #:coeff 100)))
                )
                (unless (zero? (c "rtx-blok" id))
                            (tempo (string-append "rtx" Name) 0 0))
                (unless (zero? (c "rty-blok" id))
                            (tempo (string-append "rty" Name) 0 0))
                (unless (zero? (c "rtz-blok" id))
                            (tempo (string-append "rtz" Name) 0 0))
                (let*
                    (
                        (sensx (- (* 2 (c "sens-x-2" id)) 1))
                        (sensy (- (* 2 (c "sens-y-2" id)) 1))
                        (sensz (- (* 2 (c "sens-z-2" id)) 1))
                        (coeffrx 360)
                        (coeffry 360)
                        (coeffrz 360)
                        (rx (c "rx" id #:coeff coeffrx))
                        (ry (c "ry" id #:coeff coeffry))
                        (rz (c "rz" id #:coeff coeffrz))
                        (rtx (* sensx (tempo (string-append "rtx" Name))))
                        (rty (* sensy (tempo (string-append "rty" Name))))
                        (rtz (* sensz (tempo (string-append "rtz" Name))))
                        (Rx (+ rtx rx))
                        (Ry (+ rty ry))
                        (Rz (+ rtz rz))
                    )
                    (with-state
                        (blur (+ (c "blur" id) 0.01))
        ;                (smoothing-bias (c "smoothing-bias" id #:coeff 2))
                        (rotate (vector Rx Ry Rz))
                        (rotate (vector 90 0 180))
                        (ligne+ 0 0 13 13)
                    )
                )
            )
        )
    )
)
;(every-frame (render))
