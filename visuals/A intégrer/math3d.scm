(clear)
(blur 0)
;(hint-points)
;(hint-vertcols)
(hint-unlit)
(define Id "003-artefact")
(define art-n 30)
(define artefact-list
    (list
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
;                    (* i (/ 0.1 (pdata-size)))
                    0.1
                )
                "w"
            )
        )
    )
    artefact-list
)

(define R 2)
(define r 1)
;(define a 1)
;(define b 2)
;(define c 3)
(define v 1)
;(define e .1)

(define (artefact id cross)
    (define (artefact-sub n m)
        (when (equal? Id id)
            (let
                (
                    (prim (cdr (list-ref artefact-list (- n 1))))
                    (a (mn 0 40 5))
                    (b (mn 0 41 5))
                    (c (mn 0 42 10))
                    (e (mn 0 43))
                )
                (with-primitive prim
                            (identity)

;                            (rotate (vmul (vector (* 360 (flxrnd)) (* 360 (flxrnd)) (* 360 (flxrnd))) 1))
;                            (rotate (vmul (vector (* 360 (time-now) (flxrnd)) (* 360 (time-now) (flxrnd)) (* 360 (time-now) (flxrnd))) 0.1))
                    (pdata-index-map!
                        (lambda (i p)
                            (flxseed prim)
;    (show                        (vector (cos (+ i (time-now))) (sin (+ i (time-now))) 0))
;                            (vmul (vector (cos (+ (* i 0.1) (* 2 (time-now)))) (sin (+ (* i 0.1) (* 2 (time-now)))) 0) (+ 5 (- (* 0.2 (flxrnd)) 0.1)))
;                            (vector
;                                (* (+ R (* r (cos (+ (* i e) (* (time-now) v))))) (cos (* (time-now) v)))
;                                (* (+ R (* r (cos (+ (* i e) (* (time-now) v))))) (sin (* (time-now) v)))
;                                (* r (sin (+ (* i e) (* (time-now) v))))
;                            )
                            (vector
                                (+ (* a (cos (* i e))) (* b (cos (* 3 (* i e)))))
                                (+ (* a (sin (* i e))) (* b (sin (* 3 (* i e)))))
                                (* c (sin (* 2 (* i e))))
                            )
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
