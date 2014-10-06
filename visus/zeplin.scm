(define zeplin-plane #f)
(define (zeplin-build id)
    (with-state
        (texture (load-texture "ballon.png"))
        (set! zeplin-plane (build-plane))
        (with-primitive zeplin-plane
            (pdata-set "p" 0 (vector -8 -6 0))
            (pdata-set "p" 1 (vector 8 -6 0))
            (pdata-set "p" 2 (vector 8 6 0))
            (pdata-set "p" 3 (vector -8 6 0))
            
            (pdata-set "p" 7 (vector -8 -6 0))
            (pdata-set "p" 6 (vector 8 -6 0))
            (pdata-set "p" 5 (vector 8 6 0))
            (pdata-set "p" 4 (vector -8 6 0))
)
        )
    )

(define (zeplin-destroy id)
    (destroy zeplin-plane)
    )

(define (zeplin id cross)
    (with-primitive zeplin-plane
        (identity)
        (hint-unlit)
        (hint-ignore-depth)
        (translate (vmul (vsub (vector (c "trans-x" id) (c "trans-y" id) (c "trans-z" id)) (vector .5 .5 .5)) (* 20 (c "trans-coeff" id))))
        (rotate (vector (* 5 (sin (* 1 (time)))) (* 7 (cos (+ 12 (* 0.5 (time))))) (* 3 (sin (+ 3 (* 0.2 (time)))))))
)
    ) 
