(clear)
(define square-text-size 50)
(define square-text (build-pixels square-text-size square-text-size))
    (with-primitive square-text
        (pdata-index-map!
            (lambda (i c)
                (cond
                    ((< i square-text-size)
                        (vector 1 1 1)
                    )
                    ((> i (- (* square-text-size square-text-size) square-text-size))
                        (vector 1 1 1)
                    )
                    ((zero? (remainder i square-text-size))
                        (vector 1 1 1)
                    )
                    ((= (- square-text-size 1) (remainder i square-text-size))
                        (vector 1 1 1)
                    )
                    (else
                        (vector 1 0 0)
                    )
                )
            )
            "c"
        )
        (pixels-upload)
    )

(define square-wall
    (with-primitive (build-plane)
        (texture (pixels->texture square-text))
    )
)

(define (square-text-wall id cross)
    (with-primitive square-text
        (pdata-index-map!
            (lambda (i c)
                (let
                    (
                        (color-out (hsv->rgb (vector 0.6 1 (gh 1))))
                        (color-in (hsv->rgb (vector 0.27 1 (* (gh 2) 4) .3)))
                    )
                (cond
                    ((< i square-text-size)
                        (hsv->rgb (vector 0.6 1 (gh 1)))
                        color-out
                    )
                    ((> i (- (* square-text-size square-text-size) square-text-size))
                        color-out
                    )
                    ((zero? (remainder i square-text-size))
                        color-out
                    )
                    ((= (- square-text-size 1) (remainder i square-text-size))
                        color-out
                    )
                    (else
                        color-in
                    )
                )
                )
            )
            "c"
        )
        (pixels-upload)
    )
)

(every-frame (square-text-wall 1 1)) 