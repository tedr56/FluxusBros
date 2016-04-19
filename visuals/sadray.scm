(define sadray-list (make-hash))

(define sadray-points 15)
(define sadray-prims 10)

(define (sadray-prims-check id)
    (let ((sadray-primsC (floor (+ 1 (* (c "prims-num" id) 50)))))
        (unless (= sadray-primsC (length (hash-ref sadray-list id)))
            (cond
                ((> sadray-primsC (length (hash-ref sadray-list id)))
                    (let ((newPrim (build-ribbon sadray-points)))
                        (with-primitive newPrim
                            (hint-none)
                            (hint-solid)
                            (hint-unlit)
;;                             (hint-vertcols)
                            (colour (vector 1 1 1))
                            (pdata-add "initialP" "v")
                            (pdata-index-map!
                                (lambda (i p)
                                    (vector (* (- i (/ (pdata-size) 2)) 2) 0 0)
                                )
                                "initialP"
                            )
                            (pdata-copy "initialP" "p")
                            (pdata-add "initialW" "f")
                            (let ((size-w (max 0.05 (* (flxrnd) 0.1))))
                                (pdata-map!
                                    (lambda (w)
                                        size-w
                                    )
                                    "initialW"
                                )
                            )
                            (pdata-copy "initialW" "w")
                            (colour (vector 1 1 1))
                        )
                        (hash-set! sadray-list id (append (hash-ref sadray-list id) (list newPrim)))
                    )
                )
                ((< sadray-primsC (length (hash-ref sadray-list id)))
                    (destroy (first (hash-ref sadray-list id)))
                    (hash-set! sadray-list id (drop (hash-ref sadray-list id) 1))
                )
                (sadray-prims-check id)
            )
        )
    )
)

(define (sadray-build id)
    (hash-set! sadray-list id '())
    (sadray-prims-check id)
)
(define (sadray id cross)
    (flxseed 1)
    (sadray-prims-check id)
    (let*
        (
            (points (c "points" id #:type "Tuio"))
            (avoid-radius (c "avoid-radius" id))
            (speed (* (c "speed" id) 10))
            (xLimit (* (* (- (c "xLimit" id) 0.5) 2) 10))
            (zLimit (* (c "zLimit" id) 10))
        )
        (for-each
            (lambda (prim)
                (with-primitive prim
                    (let*
                        (
                            (prim-speed
                                (if (> (vector-ref (pdata-get "initialP" 0) 2) zLimit)
                                    (* -2 zLimit)
                                    (* (flxrnd) (delta) speed)
                                )
                            )
                            (ga-vec (ga))
                            (ga-size (vector-length ga-vec))
                        )
                        (pdata-op "+" "initialP" (vector 0 0 prim-speed))
                        (pdata-copy "initialP" "p")
                        (pdata-index-map!
                            (lambda (i p)
                                (vadd p (vector 0 0 (* (vector-ref ga-vec (modulo (inexact->exact (floor (/ (pdata-size) (max 1 i)))) ga-size)) 10)))
                            )
                            "p"
                        )
                        
;;                         (pdata-index-map!
;;                             (lambda (i p)
;;                                 (
;;                             )
;;                             "p"
;;                         )
                    )
                )
            )
            (hash-ref sadray-list id)
        )
    )
)