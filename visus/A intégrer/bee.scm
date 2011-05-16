(require scheme/list)

(define bee-prims (make-hash))

(define (bee-destroy id)
    (for-each
        (lambda (l)
            (for-each
                (lambda (n)
                    (destroy (car n))
                )
                l
            )
        )
        (hash-ref bee-prims id)
    )
    (hash-remove! bee-prims id)
)

(define (bee id cross)
;
; (((1 v) (2 v) (3 v)) ((4 v) (5 v) (6 v)))
;
    (letrec
        (
            (bee-build
                (lambda (n)
                    (unless (zero? n)
                        (let*
                            (
                                (new-torus  (bee-build-torus 0 10 '()))
                                (new-list (append (hash-ref bee-prims id) (list new-torus)))
                            )
(show new-list)
                            (hash-set! bee-prims id new-list)
                            (bee-build (- n 1))
                        )
                    )
                )
            )
            (bee-build-torus
                (lambda (i i-max l)
                    (cond
                        ((= i i-max)
                            l
                        )
                        (else
                            (with-state
                                (cond
;Trouver la bonne formule pour Cylindric->cartesien
                                    ((and (zero? i) (not (empty? (hash-ref bee-prims id))))
                                        (parent (bee-previous-torus))
                                        (translate (cylindric->cartesien (vector 3 0 2)))
                                    )
                                    ((not (empty? (hash-ref bee-prims id)))
                                        (parent (car (list-ref l (- (length l) 1))))
                                        (translate (cylindric->cartesien (vector 3 (* i (/ 360 i-max)) 2)))
                                    )
                                )
                                (let*
                                    (
                                        (new-prim (build-cube))
                                        (new-element (cons new-prim (vector 0 0 0)))
                                        (new-list (append l (list new-element)))
                                    )
;(show "debug list")
;(show l)
;(show new-element)
;(show new-list)
                                    (bee-build-torus (+ i 1) i-max new-list)
                                )
                            )
                        )
                    )
                )
            )
            (bee-previous-torus
                (lambda ()
                    (let*
                        (
                            (bps (hash-ref bee-prims id))
                            (last-torus (list-ref bps (- (length bps) 1)))
                        )
(show last-torus)
                        (car (car last-torus))
                    )
                )
            )
            (bee-animate
                (lambda ()
                    1
                )
            )
        )
        (unless (hash-has-key? bee-prims id)
            (hash-set! bee-prims id '())
            (bee-build 10)
        )
        (bee-animate)
    )
)