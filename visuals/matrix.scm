;matrix

(define (matrix id cross)
    (letrec
        (
            (draw-row
                (lambda (x y z)
                    (cond 
                        ((not (zero? x))
                            (translate (vector 1 0 0))
                            (with-state  
                                (let*
                                    (
                                        (vec (vtransform (vector 0 0 0) (get-transform)))
                                        (dist (vdist vec (vector 5 (* 5 (+ 1 (sin (time-now)))) 5)))
                                        (size (/ (max 0 (- 5 dist)) 5))
                                        (n 0)
                                    )


                                    ;mouvement de la camera
                                    #(set-camera-transform
                                        (mtranslate
                                            (vector 
                                                (* (- (* 2 (c "set-camera-transform-x" id)) 1) 100) 
                                                (* (- (* 2 (c "set-camera-transform-y" id)) 1) 100)
                                                (* (c "set-camera-transform-z" id) -500)
                                            )
                                        )
                                    )

                                    ;rotation par rapport au temps+midi
                                    (rotate (vector 
                                            (* (+ (* (c "rotate-x-time" id) (time2))(c "rotate-x" id)) 90) 
                                            (* (+ (* (c "rotate-y-time" id) (time2))(c "rotate-y" id)) 360)  
                                            (* (+ (* (c "rotate-z-time" id) (time2))(c "rotate-z" id)) 360)))

                                    ;(set-ortho-zoom (* (c "set-ortho-zoom" id) -50))

                                    (blur (+ (c "blur" id) 0.01))


                                    ;couleur faces
                                    (colour (vector 
                                            (* (* 0.0005 (* (c "colour-r" id) (gh 2))) (+ 1 n))
                                            (* (* 0.003 (* (c "colour-g" id) (gh 8))) (+ 1 n))
                                            (* (* 0.001 (* (c "colour-r" id) (gh 15))) (+ 1 n))))
                                    ;couleur et epaisseur des arretes
                                    (wire-colour(vector 
                                            (* (* 0.001 (* (c "wire-colour-r" id) (gh 2))) (+ 1 n))
                                            (* (* 0.003 (* (c "wire-colour-r" id) (gh 8))) (+ 1 n))
                                            (* (* 0.001 (* (c "wire-colour-r" id) (gh 15))) (+ 1 n))))

                                    (line-width (+ 0.1 (* 10 (c "line-width" id))))

                                    ;affichage des faces ou des arrtes
                                    (cond ((= 1 (c "wire" id))
                                        (hint-none)(hint-wire)))
                                    (cond ((= 1 (c "wire-stippled" id))
                                        (hint-none)(hint-wire-stippled)))
                                    (cond ((= 1 (c "solid" id))
                                        (hint-none)(hint-solid)))


                                    (scale (vector size size size))
                                    (draw-cube)

                                    (draw-row (- x 1) y z)
                                )
                            )
                        )
                    )
                )
            )
            (draw-flat-grid
                (lambda (x y z)
                    (cond
                        ((not (zero? y))
                            (translate (vector 0 1 0))
                            (with-state (draw-row x y z))           
                            (draw-flat-grid x (- y 1) z))
                    )
                )
            )

            (draw-grid
                (lambda (x y z)
                    (cond 
                        ((not (zero? z))
                            (translate (vector 0 0 1))
                            (with-state (draw-flat-grid x y z))
                            (draw-grid x y (- z 1))
                        )
                    )
                )
            )
        )
        (translate
            (vector 
                (* (- (* 2 (c "set-camera-transform-x" id)) 1) 100) 
                (* (- (* 2 (c "set-camera-transform-y" id)) 1) 100)
                (* (c "set-camera-transform-z" id) -500)
            )
        )
        (with-state
            (translate
                (vector 
                    (* (- (* 2 (c "set-camera-transform-x" id)) 1) 500) 
                    (* (- (* 2 (c "set-camera-transform-y" id)) 1) 500)
                    (* (c "set-camera-transform-z" id) -1000)
                )
            )
            (draw-grid 10 10 10)
        )
    )
)

;(every-frame (draw-grid 10 10 10)) 
