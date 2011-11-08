;Equal01

(clear)

;(ortho)
(persp)

(define (render n)

(gain (* 20 (midi-ccn 0 26)))

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

(push)
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

;translation x,y,z par rapport au sinus et au temps
(translate  (vector 
        ;(* (midi-cc 0 20) (sin (+ (* (* 10 (midi-ccn 0 21)) (time2)) n)))
        0
        (* (midi-cc 0 20) (sin (+ (* (* 10 (midi-ccn 0 21)) (time2)) n))) 
        ;(* (midi-cc 0 20) (sin (+ (* (* 10 (midi-ccn 0 21)) (time2)) n)))
        0
        ))

(translate  (vector (* 8 -2.4) 0 0) )
(translate (vmul (vector n 0 0) (* 1.2 (* 10 (midi-ccn 0 27)))))
(scale (vector 
        (* 10 (midi-ccn 0 27)) 
        (* 0.05 (* (midi-ccn 0 25) (gh n))) 
        (* 50 (midi-ccn 0 19))))
(draw-cube)
(pop)

(push)
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

;translation x,y,z par rapport au sinus et au temps
(translate  (vector 
        ;(* (midi-cc 0 20) (sin (+ (* (* 10 (midi-ccn 0 21)) (time2)) n)))
        0
        (* (midi-cc 0 20) (sin (+ (* (* 10 (midi-ccn 0 21)) (time2)) n))) 
        ;(* (midi-cc 0 20) (sin (+ (* (* 10 (midi-ccn 0 21)) (time2)) n)))
        0
        ))

(translate  (vector (* 8 -2.4) 0 0) )
(translate (vmul (vector n 0 0) (* -1.2 (* 10 (midi-ccn 0 27)))))
(scale (vector 
        (* 10 (midi-ccn 0 27)) 
        (* 0.05 (* (midi-ccn 0 25) (gh n))) 
        (* 50 (midi-ccn 0 19))))
(draw-cube)
(pop)

(if (= (midi-ccn 2 43) 1)
   (reset-camera)
   (+ 0 0))

(if (= n 0)
   (+ 0 0)
   (render (- n 1))))

(define (render2 n)
(rotate (vector  (* (midi-ccn 0 6) 360) (* (midi-ccn 0 1) 360)  (* (midi-ccn 0 14) 360)))
(rotate (vector  (* (time2) (* (midi-ccn 0 16) 360)) (* (time2) (* (midi-ccn 0 17) 360))  (* (time2) (* (midi-ccn 0 18) 360))))
(render n)
)
(every-frame (render2 16))