(define (spectre id cross)
    (let*
        (
;            (long (vector-length (ga)))
            (Name (send id get-name))
            (long 128)
            (move-gain 4)
            (num-id
                (+
                    (string->number (substring Name 0 3))
                    (string->number (substring Name 4))
                )
            )
        )
        (letrec
            (
                (spectral-line
                    (lambda ((n 0))
                        (unless (>= n (- long 1))
                            (push)
(flxseed 2)
(rotate (vector 0 0 (* 50 (+ (time) (* (flxrnd) 1000 num-id)))))
                            (blur 0.3)
                            (translate (vector 0 -2 (+ (* -1 n) 10)))
                            (wire-colour (hsv->rgb (vector (* n (/ n long)) 1 1)))
                            (line-width 10)
                            (draw-line
                                (vector (* move-gain (vector-ref (ga) n)) 0 -0.5)
                                (vector (* move-gain (vector-ref (ga) (+ n 1))) 0 0.5)
                            )
                            (pop)
                            (spectral-line (+ n 1))
                        )
                    )
                )
            )
            (spectral-line)
        )
    )
) 
