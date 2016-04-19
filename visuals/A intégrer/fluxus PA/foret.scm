;foret

(clear)

(define (ligne x y maxX)


(let (  (color
        (vector 
        (* (* 10.1 (* (midi-ccn 0 28) (gh 2))) (+ (flxrnd) 0.01)) 
        (* (* 0.3 (* (midi-ccn 0 29) (gh 8))) (+ (flxrnd) 0.01)) 
        (* (* 0.5 (* (midi-ccn 0 30) (gh 15))) (+ (flxrnd) 0.01)))) 
        (wire-color
        (vector
        (* (* 5.1 (* (midi-ccn 0 28) (gh 2))) (+ 1 1)) 
        (* (* 0.05 (* (midi-ccn 0 29) (gh 8))) (+ 1 1)) 
        (* (* 0.5 (* (midi-ccn 0 30) (gh 15))) (+ 1 1)))) 
        (line-ep (+ 0.1 (* 10 (midi-ccn 0 31)))))
     


(cond    ((= 1 (midi-ccn 2 81))
    (hint-none)(hint-wire))
        ((= 1 (midi-ccn 2 71))
    (hint-none)(hint-wire-stippled))
        ((= 1 (midi-ccn 2 61))
    (hint-none)(hint-solid)))

    (push)
    (translate (vector (* 4 x) y 0))
    (colour color)
    (wire-colour wire-color)
    (line-width line-ep)
    (let ((aleatoire (gh (* (flxrnd)  12))))
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
    (let ((aleatoire (gh (* (flxrnd)  12))))
    (translate (vector 0 0 (* -0.5 aleatoire)))
    (scale (vector 1 1 aleatoire) )

) 
    (draw-cube)
    (pop)
)
 (when (< x maxX)
        (ligne (+ x 1) y maxX))

)


(define (ligne+ x y maxX maxY)
    (push)
    (translate (vector 0 (* 4 y) 0))
    (ligne x y maxX)
    (pop)
    (push)
    (translate (vector 0 (* -4 y) 0))
    (ligne x y maxX)
    (pop)

  (when (< y maxY)
         (ligne+ x (+ y 1) maxX maxY))
)


(define (render)
(flxseed 2)
(blur (+ (midi-ccn 0 24) 0.01))
(gain (* 0.1 (midi-ccn 0 27)))
(smoothing-bias (mn 0 26 2))
(let* (  (coeffx 200)
         (coeffy 100)
         (coeffz 300)
         (tx (mn 0 7 coeffx))
         (ty (mn 0 0 coeffy))
         (tz (mn 0 15 coeffz))
        (Tx (- tx (* 0.5 coeffx)))
        (Ty (- ty (* 0.5 coeffy)))
        (Tz (- tz (* 0.5 coeffz)))
      )
(translate (vector Tx Ty Tz))
)


(let (     (sensx (- (* 2 (mn 2 41)) 1))
            (sensy (- (* 2 (mn 2 33)) 1))
            (sensz (- (* 2 (mn 2 23)) 1))
)


(tempo "rtx" (* sensx (mn 0 5 100)))
(tempo "rty" (* sensy (mn 0 2 100)))
(tempo "rtz" (* sensz (mn 0 13 100)))
)
(unless (zero? (m 2 83))
            (tempo "rtx" 0 0))
(unless (zero? (m 2 73))
            (tempo "rty" 0 0))
(unless (zero? (m 2 63))
            (tempo "rtz" 0 0))


        
(let* (     (sensx (- (* 2 (mn 2 13)) 1))
            (sensy (- (* 2 (mn 2 3)) 1))
            (sensz (- (* 2 (mn 2 43)) 1))
            (coeffrx 360)
            (coeffry 360)
            (coeffrz 360)
            (rx (mn 0 6 coeffrx))
            (ry (mn 0 1 coeffry))
            (rz (mn 0 14 coeffrz))
            (rtx (* sensx (tempo "rtx")))
            (rty (* sensy (tempo "rty")))
            (rtz (* sensz (tempo "rtz")))
                (Rx (+ rtx rx))
                (Ry (+ rty ry))
                (Rz (+ rtz rz))
      )
(rotate (vector Rx Ry Rz))
)
(rotate (vector 90 0 180))
;(translate (vector 0 52 0))
    (ligne+ 0 0 13 13)


)

(every-frame (render))