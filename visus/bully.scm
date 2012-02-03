(define bully-hash (make-hash))
(define bully-seed (make-hash))

(define (bully-destroy id)
    (destroy (hash-ref bully-hash id))
    (hash-remove! bully-hash id)
)

(define (bully id cross)
    (unless (hash-has-key? bully-hash id)
        (hash-set! bully-hash id (build-disk 20))
        (hash-set! bully-seed id (+ (vector-ref (midi-position) 0) (vector-ref (midi-position) 1)))
    )
    (let
        (
            (name (send id get-name))
            (b (c "blur" id))
            (g (c "gain" id))
        )
        (letrec
            (
                (num-id
                    (lambda ()
                        (+
                            (string->number (substring name 0 3))
                            (string->number (substring name 4))
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
                        (with-primitive (hash-ref bully-hash id)
                            (identity)
                            (blur b)
                            (hint-wire)
                            (hint-ignore-depth)
                            (line-width 20)
                            (scale (vector 1 1 1))
                            (when (beat-catch name "posrnd" #:beat (list (c "tempo" id)))
                                (hash-set! bully-seed id (+ (num-id) (inexact->exact (floor (* 1000 (gh (* 16 (flxrnd)))))) 1))
                            )
                            (flxseed (hash-ref bully-seed id))
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
