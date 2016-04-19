(clear)

(define sparkle-prims (make-hash))

(define (sparkle-build id)
    (hash-set!
        sparkle-prims
        id
        (list
            (build-cylinder 30 10)
            (build-list
                10
                (lambda (x)
                    (build-cylinder 1 10)
                    )
                )
            )
        )
    (let
        (
            (tunnel (car (hash-ref sparkle-prims id)))
            (source (cdr (hash-ref sparkle-prims id)))
            )
        (with-primitive tunnel
            (hint-normal)
            (pdata-map!
                (lambda (n)
                    (vmul n -1)
                    )
                "n"
                )
            (scale 10)
            #(recalc-normals 0)
            )
        (for ((i source))
            (with-primitive i
                (rotate (vmul (vector (flxrnd) (flxrnd) (flxrnd)) 360))
                )
            )
        )
    )

(define (sparkle id cross)
    (unless (hash-has-key? sparkle-prims id)
        (sparkle-build id)
        )
    )

(sparkle 1 1)