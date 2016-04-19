;(clear)
;(reset-camera)

(define (apprehension-intro id cross freq i sub)
    (unless (zero? i)
        (random-seed i)
        (let
            (
                (n (random freq))
                )
            (with-state
;            (show (c "translate-g-gh" id))
                (translate (vector (* (- (random 3) 1) (gh n)) (* (* (- (random 3) 1) (gh n)) (c "translate-g-gh" id #:coeff 10)) (* (- (random 3) 1) (gh n))))
                (when (even? i)
                    (rotate (vmul (vector 0 0 (* (* 2 (- (random 2) .5)) 90)) (c "rotate-1" id)))
                    )
                (when (odd? i)
                    (rotate (vmul (vector 0 0 (* (* 2 (- (random 2) .5)) 90)) (c "rotate-2" id)))
                    )
                (translate (vector (* (- (random 3) 1) (gh n)) (* (* (- (random 3) 1) (gh n)) (c "translate-gh" id)) (* (- (random 3) 1) (gh n))))
                (scale (vector .2 1 .2))
                (scale (vector 1 (gh n) 1))
                
                (colour (vmul (vector 1 1 1) (* (c "color-gh" id #:coeff .2) (gh n))))
                (when (< i (c "red" id #:coeff 1000))
                    #(hint-ignore-depth)
                    (colour (vmul (vector 1 0 0) (* (c "color-gh" id #:coeff .2) (gh 1))))
                    )
                (if (<= sub 0)
                    (draw-cylinder)

                    (draw-plane)

                    )
                )
            (if (<= sub 0)
                (apprehension-intro id cross freq (- i 1) sub)
                (with-state
            (scale (vector sub 0.5 1))
                    (apprehension-intro id cross freq i (- sub 1))
)
                    
                )
            )
        )
    )

(define (apprehension id cross)
    (apprehension-intro id cross 16 1000 (c "sub" id #:coeff 3))
)

;(every-frame (apprehension-intro 16 1000 (mn 5 7 3)))
