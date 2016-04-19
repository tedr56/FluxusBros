(define gridtracker-prims (make-hash))
(define gridtracker-faces (make-hash))

(define (gridtracker-build id)
    (define grid (build-seg-plane 15 15))
    (with-primitive grid
        (hint-vertcols)
        (pdata-op "*" "p" (vector 10 10 0))
        (pdata-add "pos" "vec")
        (pdata-copy "p" "pos")
        (hash-set!
            gridtracker-faces
            id
            (map
                (lambda (i)
                    (let*
                        (
                            (clrrnd (rndvec))
                            (p0 (pdata-ref "p" (* i 4)))
                            (p1 (pdata-ref "p" (+ (* i 4) 1)))
                            (p2 (pdata-ref "p" (+ (* i 4) 2)))
                            (p3 (pdata-ref "p" (+ (* i 4) 3)))
                            (leftE (vadd p0 (vmul (vsub p0 p1) -1)))
                            (rightE (vadd p2 (vmul (vsub p2 p3) -1)))
                            (E (vadd leftE (vmul (vsub leftE rightE) -1)))
                        )
                        (pdata-set! "c" (* i 4) clrrnd)
                        (pdata-set! "c" (+ (* i 4) 1) clrrnd)
                        (pdata-set! "c" (+ (* i 4) 2) clrrnd)
                        (pdata-set! "c" (+ (* i 4) 3) clrrnd)
                        E
                    )
                )
                (build-list (/ (pdata-size) 4) values)
            )
        )
        (pdata-op "+" "p" (vector 0 0 -10))
    )
    (hash-set! gridtracker-prims id grid)
)

(define (gridtracker-destroy id)
    (destroy (hash-ref gridtracker-prims id))
    (hash-remove! (hash-ref gridtracker-prims id))
    (hash-remove! (hash-ref gridtracker-faces id))
)

(define (gridtracker id cross)
    (let*
        (
            (tuioControl (c "points" id #:type "Tuio"))
            (tuioCursors (PersistentTuioCursor tuioControl (* (c "persistTuioDelay" id) 10)))
        )
        ;(unless (empty? tuioCursors)
            (let*
                (
                    (tuioPoints (CalibrateTuioCursors tuioCursors (* 10 (c "tuioX" id)) (* 10 (c "tuioY" id))))
                    (radius (* 1 (c "point-radius" id)))
                    (speed-up  (c "speed" id))
                    (faces (hash-ref gridtracker-faces id))
                    (add-z speed-up)
                    (sub-z (- 0 (/ speed-up 2)))
                    (sub-vector (vector 0 0 sub-z))
                    (max-vector (vector 0 0 0))
                    (min-vector (vector 0 0 -10))
                )
                (with-primitive (hash-ref gridtracker-prims id)
                    (for-each
                        (lambda (i)
                            (let
                                (
                                    (face (list-ref faces i))
                                    (p0 (* 4 i))
                                )
                                (for-each
                                    (lambda (cursor)
                                        (show "debug")
                                        (show face)
                                        (let*
                                            (
                                                (dist (vdist-sq face cursor))
                                                (add-vector sub-vector)
                                                (add-vector-safe
                                                    (lambda (pn)
                                                        (maxv
                                                            max-vector
                                                            (minv
                                                                min-vector
                                                                (vadd
                                                                    pn
                                                                    add-vector
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                            (when (< dist radius)
                                                (let*
                                                    (
                                                        (point (vector 0 0 (vector-ref face 2)))
                                                        (p (pdata-ref "p" pn))
                                                        (add-radius (vector 0 0 (max 0 (- 1 (* speed-up 0.00001 dist)))))
                                                        (add-vector-tuio
                                                        
                                                    )
                                                    (set! add-vector add-radius)
                                                )
                                            )
                                            (pdata-set! "p" p0 (add-vector-safe (pdata-ref "p" p0)))
                                            (pdata-set! "p" (+ p0 1) (add-vector-safe (pdata-ref "p" (+ p0 1))))
                                            (pdata-set! "p" (+ p0 1) (add-vector-safe (pdata-ref "p" (+ p0 2))))
                                            (pdata-set! "p" (+ p0 1) (add-vector-safe (pdata-ref "p" (+ p0 3))))
                                            (vector-set! faces i (vector (vector-ref face 0) (vector-ref face 1) (vector-ref add-vector 2)))
                                        )
                                    )
                                    tuioPoints
                                )
                            )
                        )
                        (build-list (length faces) values)
                    )
                )
            )
;        )
    )
)
