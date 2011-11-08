;Equal01

(define (equal_basse_sub n id)
    (let
        (
            (g (c "gain" id #:coeff 20))
        )

;mouvement de la camera
        (set-camera-transform
            (mtranslate
                (vector
                    (* (- (* 2 (c "cam-x" id)) 1) 100)
                    (* (- (* 2 (c "cam-y" id)) 1) 100)
                    (* (c "cam-z" id) -500)
                )
            )
        )

;rotation par rapport au temps+midi
        (rotate
            (vector 
                (* (+ (* (c "R-L-X-time" id) (time2)) (c "R-L-X-coeff" id)) 90) 
                (* (+ (* (c "R-L-Y-time" id) (time2)) (c "R-L-Y-coeff" id)) 360)  
                (* (+ (* (c "R-L-Z-time" id) (time2)) (c "R-L-Z-coeff" id)) 360)
            )
        )

        (set-ortho-zoom (* (c "ortho-zoom" id) -50))

        (blur (+ (c "blur" id) 0.01))

        (push)

;couleur faces
        (colour
            (vector 
                (* (* 0.0005 (* (c "clr-r" id) (gh2 2))) (+ 1 n)) 
                (* (* 0.003 (* (c "clr-g" id) (gh2 8))) (+ 1 n)) 
                (* (* 0.001 (* (c "clr-b" id) (gh2 15))) (+ 1 n))
            )
        )

;couleur et epaisseur des arretes
        (wire-colour 
            (vector 
                (* (* 0.001 (* (c "clr-r" id) (gh2 2))) (+ 1 n)) 
                (* (* 0.003 (* (c "clr-g" id) (gh2 8))) (+ 1 n)) 
                (* (* 0.001 (* (c "clr-b" id) (gh2 15))) (+ 1 n))
            )
        )

        (line-width (+ 0.1 (* 10 (c "line-width" id))))

;affichage des faces ou des arrtes
        (cond ((= 1 (c "hint-wire" id))
            (hint-none)(hint-wire)))
        (cond ((= 1 (c "hint-wire-strippled" id))
            (hint-none)(hint-wire-stippled)))
        (cond ((= 1 (c "hint-solid" id))
            (hint-none)(hint-solid)))

;translation x,y,z par rapport au sinus et au temps
        (translate
            (vector 
                0
                (* (c "trans-y-sin-coeff" id #:coeff 200) (sin (+ (* (* 10 (c "trans-y-sin" id)) (time2)) n))) 
                0
            )
        )

        (push)
            (translate (vector (* 8 -2.4) 0 0))
            (translate (vmul (vector n 0 (* 0.0009 (gh 2))) (* 1.2 (* 10 (c "trans-coeff" id)))))
            (scale
                (vector 
                    (* 10 (c "scale-x" id))
                    (* 0.05 (* (c "scale-y-gh" id) (gh2 n)))
                    (* 50 (c "scale-z" id))
                )
            )
            (draw-cube)
        (pop)

        (pop)


        (if (= (c "reset-camera" id) 1)
           (reset-camera)
           (+ 0 0))

        (unless (zero? n)
            (equal_sym_sub (- n 1) id))
    )
)

(define (equal_basse id cross)
    (push)
        (rotate (vector  (* (c "R-G-X" id) 360) (* (c "R-G-Y" id) 360)  (* (c "R-G-Z" id) 360)))
        (rotate (vector  (* (time2) (* (c "R-G-X-time" id) 360)) (* (time2) (* (c "R-G-Y-time" id) 360))  (* (time2) (* (c "R-G-Z-time" id) 360))))
        (equal_sym_sub 16 id)
    (pop)
)
;(every-frame (render2 16))
