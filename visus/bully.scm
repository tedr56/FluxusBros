;(define bully-prim (build-disk 20))
;(with-primitive bully-prim (hide 1))
;(with-primitive bully-prim (scale 0))
(define bully-hash (make-hash))

(define (bully-destroy id)
    (destroy (hash-ref bully-hash id))
    (hash-remove! bully-hash id)
)

(define (bully id cross)
;(show "")
;(show id)
;(show bully-hash)
;(show (string? id))
    (unless (hash-has-key? bully-hash id)
        (hash-set! bully-hash id (build-disk 20))
    )
    (let
        (
            (b (c "blur" id))
            (g (c "gain" id))
        )
        (letrec
            (
                (num-id
                    (lambda ()
                        (+
                            (string->number (substring id 0 3))
                            (string->number (substring id 4))
                        )
                    )
                )
                (flxrndX
                    (lambda ()
;                        (flxseed (inexact->exact (round (+ (num-id) (time)))))
                        (flxseed (+ (num-id) (vector-ref (midi-position) 0) (vector-ref (midi-position) 1)))
                        (flxrnd)
                    )
                )
                (flxrndY
                    (lambda ()
;                        (flxseed (inexact->exact (round (+ .5 (num-id) (time)))))
                        (flxseed (+ (num-id) (vector-ref (midi-position) 0)))
                        (flxrnd)
                    )
                )
                (flxrndposX       
                    (lambda ()
                        (* (- (* 2 (flxrndX)) 1) 10)
                    )
                )
                (flxrndposY
                    (lambda ()
                        (* (- (* 2 (flxrndY)) 1) 6)
                    )
                )
                (posrnd
                    (lambda ()
                        (translate (vector (flxrndposX) (flxrndposY) 0))
                    )
                )
                (draw
                    (lambda ()
;                        (push)
                        (with-primitive (hash-ref bully-hash id)
                            (identity)
;                            (blur 0.4)
(blur b)
                            (hint-wire)
                            (hint-ignore-depth)
                            (line-width 20)
                            (scale (vector 1 1 1))
                            (posrnd)
                            (scale (gl 5 g))
                            (colour (hsv->rgb (vector (flxrndY) 1 (* .5 (gl 5 g)))))
                            (wire-colour (hsv->rgb (vector (flxrndY) 1 1)))
                            (opacity (* .1 (gl 5 g)))
                            (wire-opacity (gl 5 g))
;                        (draw-instance bully-prim)
;                        (pop)
                        )
                    )
                )
            )
            (draw)
;            (show threshold)
        )
    )
)

;(every-frame (bully 1 1))