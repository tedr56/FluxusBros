(define (cube id cross)
    (with-state
        (scale (vadd (vector 1 1 1) (vector (c "size-x" id #:coeff 0.5) (c "size-y" id #:coeff 0.5) 0)))
        (draw-cube)
    )
)
