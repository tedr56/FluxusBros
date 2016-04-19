;wasup

(require scheme/math)

(define bevel (load-primitive "bevel.obj"))
(with-primitive bevel (hide 1))

(define wassup-prims (make-hash))
(define  lpoints 50)
(define  lsize 0.1)

(define (wassup-build id)
    (hash-set! wassup-prims id (make-hash))
    (let ((hash-id (hash-ref wassup-prims id)))
        (hash-set! hash-id "l1" (build-ribbon lpoints))
        (hash-set! hash-id "l2" (build-ribbon lpoints))
        (hash-set! hash-id "l3" (build-ribbon lpoints))
        (hash-set! hash-id "l4" (build-ribbon lpoints))
        (hash-set! hash-id "l5" (build-ribbon lpoints))
        (hash-set! hash-id "l6" (build-ribbon lpoints))
        (hash-set! hash-id "l7" (build-ribbon lpoints))
        (hash-set! hash-id "l8" (build-ribbon lpoints))
        (hash-set! hash-id "l9" (build-ribbon lpoints))
        (hash-set! hash-id "l10" (build-ribbon lpoints))
        (hash-set! hash-id "l11" (build-ribbon lpoints))
        (hash-set! hash-id "l12" (build-ribbon lpoints))
        (hash-for-each
            hash-id
            (lambda (k prim)
                (with-primitive prim (pdata-map! (lambda (w) lsize) "w"))
                (with-primitive prim (hint-unlit) (colour (vector 0 1 0)))
            )
        )
    )
)

(define (wassup-destroy id)
    (hash-for-each
        (hash-ref wassup-prims id)
        (lambda (k v)
            (destroy v)
        )
    )
    (hash-remove! wassup-prims id)
)

(define (wassup id cross)
    (letrec
        (
            (increment
                (lambda (i o f r s clr)
                    (for ((n (build-list (inexact->exact (abs (floor i))) values)))
                        (with-state
                            (colour (hsv->rgb (vector (+ clr (* (/ (* n o 180) (* pi r)) (c "color-gain" id #:coeff 0.2))) 1 1)))
                            (translate (vmul (vector (cos (/ (* n o 180) (* pi r))) (sin (/ (* n o 180) (* pi r))) 0) r))
                            (rotate (vector 0 0 (* (/ 360 (* 2 pi)) (/ (* n o 180) (* pi r)))))
                            (scale s)
                            (cond ((zero? (c "prim-select" id))
                                    (draw-cube))
                                  (else
                                    (scale 0.5)
                                    (draw-instance bevel))
                            )
                        )
                    )
                )
            )
            (jauge
                (lambda (rayon frequence angle taille coeff-son color lprim1-1 lprim1-2 lprim2-1 lprim2-2)
                    (let*
                        (
                            (size-cube (+ 0.001 (c "size-cube" id #:coeff 0.1)))
                            (angle-size 1)
                            (nb-cube (/ (* angle pi rayon) (* size-cube 180)))
                            (s 0.5)
                            (cube-size (vector (c "cube-size-x" id #:coeff 2) (c "cube-size-y" id #:coeff 2) (c "cube-size-z" id #:coeff 2)))
                        )
                        (push)
                        (let
                            (
                                (Rz
                                    (if (zero? (c "Rz-1" id))
                                        90
                                        270)
                                )
                                (Rx
                                    (if (zero? (c "Rx-1" id))
                                        180
                                        0)
                                )
                            )
                            (rotate (vector Rx 0 Rz))
;(show "debug wassup")
;(show coeff-son)
;(show (gh2 frequence))
;(show (max (gh2 frequence) 0))
;(show (* (max (gh2 frequence) 0) coeff-son))
;(show nb-cube)
;(show (min (* (max (gh2 frequence) 0) coeff-son) nb-cube))
                            (increment (min (* (min (max (gh2 frequence) 0) 1000) coeff-son) nb-cube) size-cube frequence rayon cube-size color)
                            (trait lprim1-1 rayon angle (vector Rx 0 Rz) -1 (vector-ref cube-size 0) (vector-ref cube-size 1))
                            (trait lprim1-2 rayon angle (vector Rx 0 Rz) 1 (vector-ref cube-size 0) (vector-ref cube-size 1))
                        )
                        (pop)

                        (push)
                        (let
                            (
                                (Rz
                                    (if (not (zero? (c "Rz-2" id)))
                                        90
                                        270)
                                )
                                (Rx
                                    (if (not (zero? (c "Rx-2" id)))
                                        180
                                        0)
                                )
                            )
                            (rotate (vector Rx 0 Rz))
                            (increment (min (* (min (max (gh2 frequence) 0) 1000) coeff-son) nb-cube) size-cube frequence rayon cube-size color)
                            (trait lprim2-1 rayon angle (vector Rx 0 Rz) -1 (vector-ref cube-size 0) (vector-ref cube-size 1))
                            (trait lprim2-2 rayon angle (vector Rx 0 Rz) 1 (vector-ref cube-size 0) (vector-ref cube-size 1))
                        )
                        (pop)
                    )
                )
            )
            (trait
                (lambda (prim Rayon Angle position side sizeX sizeY)
                    (with-primitive prim
                        (identity)
                        (let*
                            (
                                (rayon (+ Rayon (* side (* sizeX 0.5)) (* side 0.1)))
                                (offset- (atan (/ (* sizeY 0.5) rayon)))
                                (offset+ (* 2 (tan (/ (* 0.5 sizeY) rayon))))
                                (angle (+ Angle offset+))
                                (Lpoints (- lpoints 0))
                            )
                            (pdata-index-map!
                                (lambda (i p)
                                    (vmul (vector (cos (- (/ (* angle i) Lpoints) offset-)) (sin (- (/ (* angle i) Lpoints) offset-)) 0) rayon)
                                )
                                "p"
                            )
                        )
                        (rotate position)
                    )
                )
            )
            (get-prim
                (lambda (n)
                    (hash-ref (hash-ref wassup-prims id) n)
                )
            )
        )
        (blur (+ (c "blur" id) 0.01))
        (let
            (
                (rayonG (c "rayon-G" id #:coeff 20))
                (angleG (c "angle-G" id #:coeff pi))
                (g (* (c "gain-a" id #:coeff 50) (c "gain-b" id)))
                (taille (c "taille" id #:coeff 2))
            )
;            (smoothing-bias (c "smoothing-bias" id #:coeff 2))
                                                                                ;rayon frequence angle taille coeff-son color lprim1-1 lprim1-2 lprim2-1 lprim2-2
            (jauge (c "rayon-1" id #:coeff rayonG) (c "frequence-1" id #:coeff 16) (c "angle-1" id #:coeff angleG) taille (c "coeff-gain-1" id #:coeff 0.1) (c "color-1" id) (get-prim "l1") (get-prim "l2") (get-prim "l3") (get-prim "l4"))
            (jauge (c "rayon-2" id #:coeff rayonG) (c "frequence-2" id #:coeff 16) (c "angle-2" id #:coeff angleG) taille (c "coeff-gain-2" id #:coeff 0.1) (c "color-2" id) (get-prim "l5") (get-prim "l6") (get-prim "l7") (get-prim "l8"))
            (jauge (c "rayon-3" id #:coeff rayonG) (c "frequence-3" id #:coeff 16) (c "angle-3" id #:coeff angleG) taille (c "coeff-gain-3" id #:coeff 0.1) (c "color-3" id) (get-prim "l9") (get-prim "l10") (get-prim "l11") (get-prim "l12"))
        )
    )
)

;(every-frame (wasup))
