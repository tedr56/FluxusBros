(define (spectre id cross)
    (let
        (
            (long (vector-length (ga)))
        )
        (letrec
            (
                (spectral-line
                    (lambda ((n 0))
                        (unless (>= n (- long 1))
                            (push)
                            (translate (vector 0 -2 n))
                            (draw-line
                                (vector (vector-ref (ga) n) 0 -0.5)
                                (vector (vector-ref (ga) (+ n 1)) 0 -0.5)
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