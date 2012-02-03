; remplacer (rot-mid chann input) par ((rot-mid name) (tempo name (c name id))
; supprimer opa-mid
; 2nde ligne correspond aux paramètres d'origines - rechercher les noms d'origine

;(circles chann cross (+ (m chann 43) 1) (* (mn chann 44) 8) (* (mn chann 8) (m chann 9)))
;(circles chann cross n param-c g)

(define (circle id cross)
    (let
        (
            (Name (send id get-name))
		    (g (c "gain" id))
            (param-n (+ 1 (round (c "param-n" id #:coeff 127))))
            (param-c (c "param-c" id #:coeff 8))
        )
        (letrec
            (
                (rot-mid
                    (lambda (name (reset null))
                        (tempo (string-append Name "-" name) (* (- (c name id) 0.5) 1270) reset)
                    )
                 )
                 
    ;    (define (increment n o)
                 (increment
                     (lambda (n o)
;(show "debug increment")
;(show n)
;(show o)
                        (with-state
                            (colour (vector (gh2 n g) 1 (gh2 (- n 3) g)))
                            (wire-colour (vector (c "wire-color" id) 0 0))
                            (opacity (c "opacity" id))
                            (scale (vmul (vector (c "scale-x" id) (c "scale-y" id) (c "scale-z" id)) (* 50 (c "scale-coeff" id))))
                            (translate (vmul (vector (cos (* n o)) (sin (* n o)) 0) 4))
                            (rotate (vector (* (c "rot-x" id) (* (/ 360 (* (asin 1) 4)) (* n o))) (* (c "rot-y" id) (* (/ 360 (* (asin 1) 4)) (* n o))) (* (c "rot-z" id) (* (/ 360 (* (asin 1) 4)) (* n o)))))
                            (scale (vector (c "scale-L-x" id) (c "scale-L-y" id) (c "scale-L-z" id)))
                            (translate (vector (c "trans" id #:coeff 127) 0 0))
                            (rotate (vector 0 (* -360 (c "rot-360-y" id)) (* -360 (c "rot-360-z" id))))
                        ;            (draw-instance circle)
                            (draw-cube)
                        )
                        (cond ((<= n 1) 0)
                        (else (increment (- n 1) o)))
                    )
                )
            )
            (blur (c "blur" id))
;    (show "debug circle")
;    (show param-n)
;    (show param-c)
            (cond
                ((= (c "switch-wire" id) 127)
                    (hint-wire)
                    (line-width 0.1))
                (else
                    (hint-none)
                    (hint-solid))
            )

            (with-state
                (let ((reset (if (positive? (c "blok-rot-1" id)) 0 null)))
                    (rotate (vector (rot-mid "rot-1-x" reset) (rot-mid "rot-1-y" reset) (rot-mid "rot-1-z" reset)))
                )
                (cond ((and (>= param-c 1) (< param-c 5))
                    (increment param-n (/ (* (asin 1) 4) param-n))
                ))
                (let ((reset (if (positive? (c "blok-rot-2" id)) 0 null)))
                    (rotate (vector (rot-mid "rot-2-x" reset) (rot-mid "rot-2-y" reset) (rot-mid "rot-2-z" reset)))
                )
                (scale (vector 0.8 0.8 1))
                (cond ((and (>= param-c 2) (< param-c 6))
                    (increment param-n (/ (* (asin 1) 4) param-n))
                ))
                (let ((reset (if (positive? (c "blok-rot-3" id)) 0 null)))
                    (rotate (vector (rot-mid "rot-3-x" reset) (rot-mid "rot-3-y" reset) (rot-mid "rot-3-z" reset)))
                )
                (scale (vector 0.8 0.8 1))
                (cond ((and (>= param-c 3) (< param-c 7))
                    (increment param-n (/ (* (asin 1) 4) param-n))
                ))
                (let ((reset (if (positive? (c "blok-rot-4" id)) 0 null)))
                    (rotate (vector (rot-mid "rot-4-x" reset) (rot-mid "rot-4-y" reset) (rot-mid "rot-4-z" reset)))
                )
                (scale (vector 0.8 0.8 1))
                (cond ((and (>= param-c 4) (< param-c 8))
                    (increment param-n (/ (* (asin 1) 4) param-n))
                ))
            )
        )
    )
)
