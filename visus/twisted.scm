(current-directory "/home/ted/Sources/git/FluxusBros/")
(require "modules/vjbros.scm")
(clear)
(define twisted-pair-prims (make-hash))
(define twisted-impair-prims (make-hash))

(define twisted-pair-gh (list 14 12 9 5 2 3 6 10 13 15))
(define twisted-impair (list 11 7 4 1 5 8 12))

(define (twisted-build id)
    (hash-set! twisted-pair-prims id '())
    (hash-set! twisted-impair-prims id '())
    (hint-wire)
    (letrec
        (
            (cube-space
                (lambda (n)
                    (* n (+ 1 (/ 2 3)))
                )
            )
            (build-cube-line
                (lambda  ((n 0) (nmax 12))
                    (unless (= n nmax)
                        (cond
                            ((even? n)
                                (build-pair-line n)
                            )
                            ((odd? n)
                                (build-impair-line n)
                            )
                        )
                        (build-cube-line (+ n 1) nmax)
                    )
                )
            )
            (build-pair-line
                (lambda (line (n 0) (nmax 10))
                    (unless (= n nmax)
                        (hash-set!
                            twisted-pair-prims id
                            (append
                                (hash-ref twisted-pair-prims id)
                                (list
                                    (with-state
                                        (translate (vector (+ (/ 2 3) (cube-space -5) (cube-space n)) (+ (cube-space -3) (cube-space (/ line 2))) 0))
                                        (scale (vector 1 1 0.01))
                                        (build-cube)))))
                        (build-pair-line line (+ n 1) nmax)
                    )
                )
            )
            (build-impair-line
                (lambda (line (n 0) (nmax 9))
                    (unless (= n nmax)
                        (hash-set!
                            twisted-impair-prims id
                            (append
                                (hash-ref twisted-impair-prims id)
                                (list
                                    (with-state
                                        (translate (vector (+  (/ -1 6) (cube-space -4) (cube-space n)) (+ (cube-space -3) (cube-space (/ line 2))) -0.01))
                                        (scale (vector 1 1 0.01))
                                        (colour (vector 1 0.7 0.7))
                                        (build-cube)))))
                        (build-impair-line line (+ n 1) nmax)
                    )
                )
            )
        )
        (build-cube-line)
    )
    (for-each
        (lambda (prim)
            (pdata-add "pos" "v")
            (pdata-copy "p" "pos")
        )
        (hash-ref twisted-pair-prims id)
    )
    (for-each
        (lambda (prim)
            (pdata-add "pos" "v")
            (pdata-copy "p" "pos")
        )
        (hash-ref twisted-impair-prims id)
    )
)

(define (twisted id cross)
    (let
        (
            (pair-prims (hash-ref twisted-pair-prims id))
            (impair-prims (hash-ref twisted-impair-prims id))
        )
        (for-each
            (lambda (n)
                (with-primitive (list-ref pair-prims n)
                    (identity)
                    (scale
                        (vector
                            (min 1 (* (gh (list-ref twisted-pair-gh (modulo n 10))) (mn 3 40)))
                            (min 1 (* (gh (list-ref twisted-pair-gh (modulo n 10))) (mn 3 41)))
                            (min 3 (* (gh (list-ref twisted-pair-gh (modulo n 10))) (mn 3 42)))
                        )
                    )
                )
            )
            (build-list (length pair-prims) values)
        )
        (for-each
            (lambda (n)
                (with-primitive (list-ref impair-prims n)
                    (identity)
                    (scale
                        (vector
                            (min 1 (* (gh (list-ref twisted-pair-gh (modulo n 10))) (mn 3 40)))
                            (min 1 (* (gh (list-ref twisted-pair-gh (modulo n 10))) (mn 3 41)))
                            (min 3 (* (gh (list-ref twisted-pair-gh (modulo n 10))) (mn 3 42)))
                        )
                    )
                )
            )
            (build-list (length impair-prims) values)
        )
    )
)
(twisted-build 1)
(every-frame (twisted 1 1))