;matrix


(define (draw-row x y z)
    (cond 
        ((not (zero? x))
            (translate (vector 1 0 0))
            (with-state  
                (let* ((vec (vtransform (vector 0 0 0) (get-transform)))
                   (dist (vdist vec (vector 5 (* 5 (+ 1 (sin (time-now)))) 5)))
                   (size (/ (max 0 (- 5 dist)) 5)))


;mouvement de la camera
(set-camera-transform (mtranslate (vector 
        (* (- (* 2 (midi-ccn 0 7)) 1) 100) 
        (* (- (* 2 (midi-ccn 0 0)) 1) 100)
        (* (midi-ccn 0 15) -500) ) )) 

;rotation par rapport au temps+midi
(rotate (vector 
        (* (+ (* (midi-ccn 0 4) (time2))(midi-ccn 0 5)) 90) 
        (* (+ (* (midi-ccn 0 3) (time2))(midi-ccn 0 2)) 360)  
        (* (+ (* (midi-ccn 0 12) (time2))(midi-ccn 0 13)) 360)))
(set-ortho-zoom (* (midi-ccn 0 7) -50))

(blur (+ (midi-ccn 0 24) 0.01))


;couleur faces
(colour (vector 
        (* (* 0.0005 (* (midi-ccn 0 28) (gh 2))) (+ 1 n)) 
        (* (* 0.003 (* (midi-ccn 0 29) (gh 8))) (+ 1 n)) 
        (* (* 0.001 (* (midi-ccn 0 30) (gh 15))) (+ 1 n)))) 
;couleur et epaisseur des arretes
(wire-colour(vector 
        (* (* 0.001 (* (midi-ccn 0 28) (gh 2))) (+ 1 n)) 
        (* (* 0.003 (* (midi-ccn 0 29) (gh 8))) (+ 1 n)) 
        (* (* 0.001 (* (midi-ccn 0 30) (gh 15))) (+ 1 n)))) 
(line-width (+ 0.1 (* 10 (midi-ccn 0 31))))

;affichage des faces ou des arrtes
(cond ((= 1 (midi-ccn 2 81))
    (hint-none)(hint-wire)))
(cond ((= 1 (midi-ccn 2 71))
    (hint-none)(hint-wire-stippled)))
(cond ((= 1 (midi-ccn 2 61))
    (hint-none)(hint-solid)))


                (scale (vector size size size)))
                (draw-cube))           
            (draw-row (- x 1) y z))))


(define (draw-flat-grid x y z)
    (cond 
        ((not (zero? y))
            (translate (vector 0 1 0))
            (with-state (draw-row x y z))           
            (draw-flat-grid x (- y 1) z))))

(define (draw-grid x y z)
    (cond 
        ((not (zero? z))
            (translate (vector 0 0 1))
            (with-state (draw-flat-grid x y z))
            (draw-grid x y (- z 1)))))

(every-frame (draw-grid 10 10 10)) 
