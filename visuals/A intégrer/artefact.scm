(clear)
(blur 0.1)
;(hint-points)
;(hint-vertcols)
(hint-unlit)
(define Id "003-artefact")
(define art-n 30)
(define artefact-list
    (list
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))
        (cons Id (build-ribbon art-n))

    )
)

(define art (build-ribbon 10))
(for-each
    (lambda (n)
        (with-primitive (cdr n)
            #(pdata-index-map!
                (lambda (i c)
                    (vector 1 1 1)
                )
                "c"
            )
            (colour (vector 1 1 1))
            (pdata-index-map!
                (lambda (i w)
                    (* i (/ 0.1 (pdata-size)))
;                    0.1
                )
                "w"
            )
        )
    )
    artefact-list
)


(define (artefact id cross)
    (define (artefact-sub n m)
        (when (equal? Id id)
            (let ((prim (cdr (list-ref artefact-list (- n 1)))))
                (with-primitive prim
                            (identity)

                            (rotate (vmul (vector (* 360 (flxrnd)) (* 360 (flxrnd)) (* 360 (flxrnd))) 1))
                            (rotate (vmul (vector (* 360 (time) (flxrnd)) (* 360 (time) (flxrnd)) (* 360 (time) (flxrnd))) 0.1))
                    (pdata-index-map!
                        (lambda (i p)
                            (flxseed prim)
;    (show                        (vector (cos (+ i (time))) (sin (+ i (time))) 0))
                            (vmul (vector (cos (+ (* i 0.1) (* 2 (time)))) (sin (+ (* i 0.1) (* 2 (time)))) 0) (+ 5 (- (* 0.2 (flxrnd)) 0.1)))
                        )
                        "p"
                    )
                )
                (unless (= n 1)
                    (artefact-sub (- n 1) m)
                )
            )
        )
    )
    (artefact-sub (length artefact-list) 3)
)

(every-frame (artefact Id 1))