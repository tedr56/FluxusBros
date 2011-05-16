(clear)
(define psyko-prim (build-pixels 70 70))
    (with-primitive psyko-prim
        (pdata-index-map! 
            (lambda (index colour)
                (let* ((x (quotient index 100)) 
                       (y (remainder index 100))
                       (d (vdist (vector 50 50 0) (vector x y 0)))
                       (v (floor (* 2 (sin (* d 0.3))))))
                    (vector v v v)))
            "c")
        (pixels-upload))
(translate (vector 2 0 0))
;(define c1 (build-torus 0.5 2 10 10))
(define c1 (build-plane))
(with-primitive c1

    (texture-params 0 '(min nearest mag nearest wrap-s repeat wrap-t repeat wrap-r repeat))

    (texture (pixels->texture psyko-prim))

    (texture-params 0 '(min nearest mag nearest wrap-s repeat wrap-t repeat wrap-r repeat))
)

(define (psyko)
       (with-primitive psyko-prim
        (pdata-index-map! 
            (lambda (index colour)
                (let* ((x (quotient index 70))
                       (y (remainder index 70))
                       (d (vdist (vector 35 35 0) (vector x y 0)))
                       (v (floor (* 2 (sin (* 4 (+ d (* 0.1 (gl 2))) (- (+ 0.3 (* 0.01 (gl 3))) (* .4 (gl 1)))))))))
;                    (vector v v v)
                    (vector (* (gl 2) (abs v)) (min 0.5 (* (abs (+ (gl 5) v)) 1)) (min 0.3 (- 1 (* (abs v) 0.2 (gl 10)))))))
            "c")
        (pixels-upload))

)
(smoothing-bias 0.95)
(set-gain! 0.7)
(every-frame (psyko)) 