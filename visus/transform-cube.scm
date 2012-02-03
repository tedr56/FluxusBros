(define transform-cube-hash (make-hash))
(define transform-cube-seed (make-hash))
(define (transform-cube-destroy id)
    (destroy (hash-ref transform-cube-hash id))
    (hash-remove! transform-cube-hash id)
    (hash-remove! transform-cube-seed id)
)

(define (transform-cube id cross)
    (unless (hash-has-key? transform-cube-hash id)
        (hash-set! transform-cube-hash id (build-cube))
        (hash-set! transform-cube-seed id (+ (vector-ref (midi-position) 0) (vector-ref (midi-position) 1)))
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
                        (flxrnd)
                    )
                )
                (flxrndY
                    (lambda ()
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
                        (with-primitive (hash-ref transform-cube-hash id)
                            (identity)
                            (blur b)
                            (hint-wire)
                            (hint-ignore-depth)
                            (line-width 20)
                            (scale (vector 1 1 1))
                            (when (beat-catch (send id get-name) "posrnd" #:beat (list (c "tempo" id)))
                                (hash-set! transform-cube-seed id (+ (num-id) (inexact->exact (floor (* 1000 (gh (* 16 (flxrnd)))))) 1))
                            )
                            (flxseed (hash-ref transform-cube-seed id))
                            (posrnd)
                            (scale (gl 5 g))
                            (colour (hsv->rgb (vector (flxrndY) 1 (* .5 (gl 5 g)))))
                            (wire-colour (hsv->rgb (vector (flxrndY) 1 1)))
                            (opacity (* .1 (gl 5 g)))
                            (wire-opacity (gl 5 g))
                        )
                    )
                )
            )
            (draw)
        )
    )
)
