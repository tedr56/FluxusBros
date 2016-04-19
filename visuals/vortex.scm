(define (vortex-destroy id)
    (travel-end id)
)
(define (vortex id cross)
    (let*
        (
            (diametre 10)
            (num-points 20)
            (voyage-start -200)
            (voyage-end 15)
            (speed 1)
            (voyage (travel id (vector 0 0 voyage-start) (vector 0 0 voyage-end) speed))
        )
        (letrec
            (
                (draw-ring
                    (lambda ()
                        (push)
                        (wire-opacity
                            (/ (+ (abs voyage-start) (vector-ref voyage 2)) (abs voyage-start))
                        )
                        (translate voyage)
;                        (when (= (vector-ref voyage 2) voyage-end)
;                            (travel-end id)
;                        )
;(show voyage)
                        (make-ring (list->vector (build-circle-points num-points diametre)))
                        (pop)
                    )
                )
                (make-ring
                    (lambda (c (n 0))
                        (wire-colour
                            (hsv->rgb
                                (vector
                                    (/ n (vector-length c))
                                    1
                                    (* .2 (gl (* (get-num-frequency-bins) (/ n (vector-length c)))))
                                )
                            )
                        )

                        (line-width 10)
                        (cond
                            ((< n (- (vector-length c) 1))
                                (draw-line
                                    (vector
                                        (vector-ref (vector-ref c n) 0)
                                        (vector-ref (vector-ref c n) 1)
                                        0
                                    )
                                    (vector
                                        (vector-ref (vector-ref c (+ n 1)) 0)
                                        (vector-ref (vector-ref c (+ n 1)) 1)
                                        0
                                    )
                                )
                                (make-ring c (+ n 1))
                            )
                            (else
                                (draw-line
                                    (vector
                                        (vector-ref (vector-ref c n) 0)
                                        (vector-ref (vector-ref c n) 1)
                                        0
                                    )
                                    (vector
                                        (vector-ref (vector-ref c 0) 0)
                                        (vector-ref (vector-ref c 0) 1)
                                        0
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (draw-ring)
            (when (= (vector-ref voyage 2) voyage-end)
                (travel-end id)
                #f
            )
        )
    )
)

;(every-frame (vortex 1 1))
        