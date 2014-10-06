(define backrain-prims (make-hash))

(define (backrain-build id)
    (define rain (build-image (load-texture "lights.png") (vector 0 0) (get-screen-size)))
    (with-primitive rain
        (pdata-op "*" "t" 5)
    )
    (hash-set! backrain-prims id rain)
)

(define (backrain-destroy id)
    (destroy (hash-ref backrain-prims id))
    (hash-remove! backrain-prims id)
)

(define (backrain id cross)
    (with-primitive (hash-ref backrain-prims id)
        (pdata-op "+" "t" (vmul (vector (* (delta) -0.3) (delta) 0) .51))
        (colour (hsv->rgb (vector 0.6 (- 1 (gh2 2 (c "gain" id #:coeff 0.1))) 1)))
    )
)
