(require scheme/list)
;(clear)
;(hint-points)

(define tennis-list (make-hash))

(define tennis-points 15)
(define tennis-prims 10)

(define (tennis-destroy id)
    (when (hash-has-key? tennis-list id)
        (for-each
            (lambda (prim)
                (destroy prim)
            )
            (hash-ref tennis-list id)
        )
    )
    (hash-remove! tennis-list id)
)

(define (tennis-build id)
;(texture (load-texture "neon.png"))
    (hash-set! tennis-list id '())
    (hash-set! tennis-list id (append (hash-ref tennis-list id) (list (build-ribbon tennis-points))))
    (hash-set! tennis-list id (append (hash-ref tennis-list id) (list (build-ribbon tennis-points))))
    (hash-set! tennis-list id (append (hash-ref tennis-list id) (list (build-ribbon tennis-points))))
    (hash-set! tennis-list id (append (hash-ref tennis-list id) (list (build-ribbon tennis-points))))
    (hash-set! tennis-list id (append (hash-ref tennis-list id) (list (build-ribbon tennis-points))))
    (hash-set! tennis-list id (append (hash-ref tennis-list id) (list (build-ribbon tennis-points))))
    (hash-set! tennis-list id (append (hash-ref tennis-list id) (list (build-ribbon tennis-points))))
    (hash-set! tennis-list id (append (hash-ref tennis-list id) (list (build-ribbon tennis-points))))
    (hash-set! tennis-list id (append (hash-ref tennis-list id) (list (build-ribbon tennis-points))))
    (hash-set! tennis-list id (append (hash-ref tennis-list id) (list (build-ribbon tennis-points))))
    (hash-set! tennis-list id (append (hash-ref tennis-list id) (list (build-ribbon tennis-points))))
    (hash-set! tennis-list id (append (hash-ref tennis-list id) (list (build-ribbon tennis-points))))

    (for-each
        (lambda (s)
            (with-primitive s
                (hint-none)
                (hint-solid)
                (hint-unlit)
                (hint-vertcols)
                (colour (vector 1 1 1))
                (pdata-index-map!
                    (lambda (i w)
                        (if (= i (- (pdata-size) 2))
                            0.7
                            (* i (/ 0.1 (pdata-size)))
                        )
                    )
                    "w"
                )
            )
        )
        (hash-ref tennis-list id)
    )
)

(define (tennis-prims-check id)
    (let ((tennis-primsC (floor (+ 1 (* (c "prims-num" id) 100)))))
        (unless (= tennis-primsC (length (hash-ref tennis-list id)))
            (cond
                ((> tennis-primsC (length (hash-ref tennis-list id)))
                    (let ((newPrim (build-ribbon tennis-points)))
                        (with-primitive newPrim
                            (hint-none)
                            (hint-solid)
                            (hint-unlit)
                            (hint-vertcols)
                            (colour (vector 1 1 1))
                            (pdata-index-map!
                                (lambda (i w)
                                    (if (= i (- (pdata-size) 2))
                                        0.7
                                        (* i (/ 0.1 (pdata-size)))
                                    )
                                )
                                "w"
                            )
                        )
                        (hash-set! tennis-list id (append (hash-ref tennis-list id) (list newPrim)))
                    )
                )
                ((< tennis-primsC (length (hash-ref tennis-list id)))
                    (destroy (first (hash-ref tennis-list id)))
                    (hash-set! tennis-list id (drop (hash-ref tennis-list id) 1))
                )
                (tennis-prims-check id)
            )
        )
    )
)

(define (tennis id cross)
    (let
        (
            (g (* (c "gain-low" id #:type "number") (* (c "gain-high" id) 5)))
            (A (* (c "a" id) 10))
            (B (* (c "b" id) 10))
            (C (* (c "c" id) 10))
            (e (+ (* (c "ecart-low" id) 0.1) (c "ecart-high" id)))
            (v (* (c "vitesse" id) 5))
            (color (+ (c "color-base" id) (* (c "color-large" id) (flxrnd))))
            (blurC (c "blur" id))
            (pos-offsetC (c "pos-offset" id))
            (size-bodyC (c "size-body" id))
            (size-headC (c "size-head" id))
            (opa-alongC (c "opa-along" id))
        )
        (tennis-prims-check id)
        (for-each
            (lambda (n)
                (let*
                    (
                        (freq (* (get-num-frequency-bins) (flxrnd)))
                        (prim n)
                        (gh-freq (gl freq g))
                    )
                    (with-primitive prim
                        (blur blurC)
                        (flxseed prim)
                        (identity)
                        (rotate (vmul (vector (* 360 (flxrnd)) (* 360 (flxrnd)) (* 360 (flxrnd))) 1))
                        ;(rotate (vmul (vector (* 360 (time) (flxrnd)) (* 360 (time) (flxrnd)) (* 360 (time) (flxrnd))) 0.1))

                        (let ((pos-offset (* (flxrnd) (* pos-offsetC 127))))
                            (pdata-index-map!
                                (lambda (i p)
                                    (vector
                                        (+ (* A (cos (+ (* i e) (* (+ pos-offset (time)) v)))) (* B (cos (* 3 (+ (* i e) (* (+ pos-offset (time)) v))))))
                                        (+ (* A (sin (+ (* i e) (* (+ pos-offset (time)) v)))) (* B (sin (* 3 (+ (* i e) (* (+ pos-offset (time)) v))))))
                                        (* C (sin (* 2 (+ (* i e) (* (+ pos-offset (time)) v)))))
                                    )
                                )
                                "p"
                            )
                        )
                        (pdata-index-map!
                            (lambda (i w)
                                (let*
                                    (
                                        (size (* (* size-bodyC 30) (* i (/ 0.1 (pdata-size)))))
                                    )
                                    (if (= i (- (pdata-size) 2))
                                        (+ (* size-headC 5) size)
                                        size
                                    )
                                )
                            )
                            "w"
                        )
                        (pdata-index-map!
                            (lambda (i t)
                                (hsv->rgb
                                    (vector
                                        color
                                        gh-freq
                                        (* i (/ 1 (pdata-size)))
                                        (* 5 opa-alongC i (/ 1 (* (pdata-size) 1)))
                                    )
                                )
                            )
                            "c"
                        )
                    )
                )
            )
            (hash-ref tennis-list id)
        )
    )
)
