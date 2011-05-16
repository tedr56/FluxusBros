(clear)
(define duel-prim (build-sphere 20 20))
(with-primitive duel-prim
    (hint-none)
    (hint-solid)
    (hint-unlit)
    (hint-vertcols)
    (hide 1)
    (pdata-copy
)

(define (duel id cross)
    (with-primitive duel-prim
        (hide 0)
        (pdata-index-map!
            (lambda (i c)
                (if (< i (* (* 20 6) (* 20 0.5)))
                    (vector 1 0 0)
                    (vector 0 0 1)
                )
            )
            "c"
        )
        (pdata-index-map!
            (lambda (i c)
                (if (< i (* (* 20 6) (* 20 0.5)))
                    (vector 1 0 0)
                    (vector 0 0 1)
                )
            )
            "p"
        )
    )
)

(every-frame (duel 22 1))