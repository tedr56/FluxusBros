(clear)
(define sphere-land-list (make-hash))

(define (sphere-land-build id)
    (let*
        (
            (new-sphere (build-sphere 100 100))
        )
        (hash-set! sphere-land-list id '())
        (hash-set! sphere-land-list id (append (hash-ref sphere-land-list id) (list new-sphere)))
        (with-primitive new-sphere
            (translate (vector 0 -40 0))
            (scale 40)
            (pdata-copy "p" "p0")
        )
    )
)


(define (sphere-land id cross)
    (unless (hash-has-key? sphere-land-list id)
        (sphere-land-build id)
    )
(show (hash-ref sphere-land-list id))
    (with-primitive (list-ref (hash-ref sphere-land-list id) 0)
        (pdata-index-map!
            (lambda (i p)
                (vmul (pdata-ref "p0" i) (gh i))
            )
            "p"
        )
    )
)

(every-frame (sphere-land  1 1)) 