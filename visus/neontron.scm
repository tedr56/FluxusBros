(require scheme/list)
;(clear)
(define neontron-bevel (load-primitive "bevel.obj"))
(define neontron-bevel-long (load-primitive "bevel-long-y.obj"))
(with-primitive neontron-bevel (hide 1))
(with-primitive neontron-bevel-long (hide 1))
(define neontron-prims (make-hash))
(define neontron-prims-plane (make-hash))
(define neontron-prims-size (make-hash))
(define bevel-prim-ratio  0.1)
(define bevel-size-ratio  (* (/ 1.8 0.3) bevel-prim-ratio))
(define bevel-ratio       (* bevel-prim-ratio 1.6))
(define bevel-long-ratio  (* bevel-prim-ratio 6.5))

(define (neontron-build id n)
    (let*
        (
            (name-rnd
                (lambda ()
                    (if (> (flxrnd) 0.5) #t #f))
                )
            (pre-name-list
                (for/list ((i (build-list n values)))
                    (name-rnd)))
            (name-conv
                (lambda (i)
                   (if i neontron-bevel neontron-bevel-long)))
            (prims-list
                (map
                    (lambda (i)
                        (with-state
                            (build-copy (name-conv i))))
                    pre-name-list))
            (build-neon-plane
                (lambda (i side)
                    (let
                        (
                            (s (if i bevel-ratio bevel-long-ratio))
                            (ss (if side -1 1))
                        )
                        (with-state
                            (hint-vertcols)
                            (colour 1)
                            (translate (vmul (vector ss 0 0) bevel-ratio))
                            (scale (vector 1 s 1))
                            (build-plane)))))
            (prims-plane-list
                (map
                    (lambda (i)
                        (list (build-neon-plane i #t) (build-neon-plane i #f))
                    )
                    pre-name-list))
        )
        (for ((i prims-list))
            (with-primitive i
;                (hint-ignore-depth)
                (recalc-normals 0)
                (colour (vector 0.7 0.7 1))
;                (colour 1)
;(hint-vertcols)
                (scale bevel-prim-ratio)
                (hide 0)
            )
        )
        (for ((i prims-plane-list))
            (with-primitive (first i)
;                (translate (vector -1 0 0))
                (pdata-index-map!
                    (lambda (i clr)
                        (vector (flxrnd) (flxrnd) (flxrnd) (flxrnd)))
                    "c"
                ))
            (with-primitive (second i)
                (pdata-index-map!
                    (lambda (i clr)
                        (vector (flxrnd) (flxrnd) (flxrnd) (flxrnd)))
                    "c"
                )))
        (hash-set! neontron-prims id prims-list)
        (hash-set! neontron-prims-plane id prims-plane-list)
        (hash-set! neontron-prims-size id pre-name-list)
    )
)
(define (neontron-destroy id)
    (when (hash-has-key? neontron-prims id)
        (for-each
            (lambda (prim)
                (destroy prim)
            )
            (hash-ref neontron-prims id)
        )
        (for-each
            (lambda (prims)
                (destroy (first prims))
                (destroy (second prims))
            )
            (hash-ref neontron-prims-plane id)
        )
    )
    (hash-remove! neontron-prims id)
    (hash-remove! neontron-prims-plane id)
    (hash-remove! neontron-prims-size id)
)
(define (neontron id cross)
    (let* ((nb-neon 60)(nb-neon-per-row 6)(nb-row (quotient nb-neon nb-neon-per-row)))
        (unless (hash-has-key? neontron-prims id)
            (neontron-build id nb-neon)
        )
        (flxseed 1)
        (blur (+ (c "blur" id) 0.1))
        (let*
            (
                (prims (hash-ref neontron-prims id))
                (planes (hash-ref neontron-prims-plane id))
                (rotate-local-ctrl (c "rotate-local-ctrl" id #:coeff 45))
                (rotate-global-ctrl (c "rotate-global-ctrl" id #:coeff 45))
                (rotate-global-plane-ctrl (c "rotate-global-plane-ctrl" id))
                (rotate-global-speed (c "rotate-global-speed" id))
                (size-row (c "size-row" id #:coeff 20))
                (size-ctrl (c "size-ctrl" id #:coeff 50))
                (size-plane-ctrl (c "size-plane-ctrl" id))
                (global-speed-ctrl (c "global-speed-ctrl" id #:coeff 1))
                (random-speed-ctrl (c "random-speed-ctrl" id #:coeff 0.5))
                (random-sens-ctrl  (c "random-sens-ctrl" id))
                (random-sens-sens-ctrl (if (positive? (c "random-sens-sens-ctrl" id)) -1 1))
                (row-sens-ctrl     (c "row-sens-ctrl" id))
                (gain-neon         (c "gain-neon" id #:coeff 0.5))
                (global-speed (* global-speed-ctrl 50))
                (random-speed
                    (lambda (i)
                        (* random-speed-ctrl (flxrnd))))
                (random-sens
                    (lambda (i) (* random-sens-sens-ctrl (if (and (zero? random-sens-ctrl) (positive? (- (flxrnd) 0.5))) 1 -1))))
                (row-sens
                    (lambda (i) 1))
                (row
                    (lambda (i) (quotient i nb-neon-per-row)))
                (rows-size  (* size-row (sub1 nb-row)))
            )
            (for ((i (build-list (length prims) values)))
                (let*
                    (
                        (neon-trans-m
                            (modulo-d
                                (* (+ global-speed (random-speed i)) (random-sens i) (row-sens i) (time))
                                size-ctrl))
                        (neon-trans (vector 0 (- neon-trans-m (/ size-ctrl 2)) 0))
                        (row-trans (vector (- (* (row i) size-row)  (/ rows-size 2)) 0 0))
                        (rotate-local (vmul (vector 0  (sin (* (time) 3 rotate-global-speed))  0) rotate-local-ctrl))
                        (rotate-global (vmul (vector (sin (* (time) 5 rotate-global-speed)) (sin (* (time) 2 rotate-global-speed)) (cos (* (time) 3 rotate-global-speed))) rotate-global-ctrl))
                    )
                    (with-primitive (list-ref prims i)
                        (identity)
                        (rotate rotate-global)
                        (translate (vadd neon-trans row-trans))
                        (scale (vector bevel-prim-ratio bevel-prim-ratio (* bevel-prim-ratio 0.5)))
                        (rotate (vmul rotate-local (gh i)))

                    )
                    (let
                        (
                            (draw-neon-plane
                                (lambda (side list-side pdata-clr)
                                    (with-primitive (list-side (list-ref planes i))
                                        (let*
                                            (
                                                (size-sort (list-ref (hash-ref neontron-prims-size id) i))
                                                (size-ratio (if size-sort bevel-ratio bevel-long-ratio))
                                                (size-rowed (+ (/ size-row 2) (* size-row size-plane-ctrl)))
                                                (size (vector size-rowed size-ratio 1))
                                                (plane-trans-side side)
                                                (plane-trans-coeff bevel-size-ratio)
                                                (plane-trans (vector (* plane-trans-coeff plane-trans-side size-rowed) 0 0))
                                            )
                                            (identity)
                                            (rotate (vmul rotate-global rotate-global-plane-ctrl))
                                            (translate (vadd neon-trans  row-trans))
                                            (rotate (vmul rotate-local (gh i)))
                                            (translate plane-trans)
                                            (scale size)
                                            (pdata-map!
                                                (lambda (clr)
                                                    (vmul (vector 0.7 0.7 1) (* (gh i) gain-neon))) "c")
                                            (map
                                                (lambda (p)
                                                    (pdata-set! "c" p
                                                        (vmul (vector 0.7 0.7 1 0) (* 0.4 (gh i) gain-neon))))
                                                pdata-clr)
                                                
                                        )
                                    )
                                )
                            )
                        )
                        (draw-neon-plane -1 first (list 3 0 4 7))
                        (draw-neon-plane 1 second (list 1 2 5 6))
                    )
                )
            )
        )
    )
)

;(every-frame (neontron 1 42))
