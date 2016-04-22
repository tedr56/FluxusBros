(require "beat-module.scm")

(define beat-sphere-prims (make-hash))
(define (beat-sphere-build id)
    (cond
        ((hash-has-key? beat-sphere-prims id)
            (beat-sphere-destroy id)
        )
        (else
            (hash-set! beat-sphere-prims id (build-icosphere 3))
            (with-primitive (hash-ref beat-sphere-prims id)
                (hint-vertcols)
                (scale 10)
(emissive (vector .1 .1 .1))
                (backfacecull 0)
            )
        )
    )
)
(define (beat-sphere-destroy id)
    (when (hash-has-key? beat-sphere-prims id)
        (destroy (hash-ref beat-sphere-prims id))
        (hash-remove! beat-sphere-prims id)
    )
)

(define (beat-sphere-swap id first-point second-point first-square second-square)
    (cond
        ((hash-has-key? beat-sphere-prims id)
            (with-primitive (hash-ref beat-sphere-prims id)
                (let*
                    (
                        (S-1-1   first-point)
                        (S-1-2   (+ first-point 1))
                        (S-1-3   (+ first-point 2))
                        (S-2-1   second-point)
                        (S-2-2   (+ second-point 1))
                        (S-2-3   (+ second-point 2))
                        (S-1-1-O (vector-ref first-square 0))
                        (S-1-2-O (vector-ref first-square 1))
                        (S-1-3-O (vector-ref first-square 2))
                        (S-2-1-O (vector-ref second-square 0))
                        (S-2-2-O (vector-ref second-square 1))
                        (S-2-3-O (vector-ref second-square 2))
                        (S-1-1-D S-2-2-O)
                        (S-1-2-D S-2-3-O)
                        (S-1-3-D S-2-1-O)
                        (S-2-1-D S-1-3-O)
                        (S-2-2-D S-1-1-O)
                        (S-2-3-D S-1-2-O)
                        (S-1-1-T (travel (string-append "S-1-1" "-" (number->string S-1-1)) S-1-1-O S-1-1-D 1))
                        (S-1-2-T (travel (string-append "S-1-2" "-" (number->string S-1-2)) S-1-2-O S-1-2-D 1))
                        (S-1-3-T (travel (string-append "S-1-3" "-" (number->string S-1-3)) S-1-3-O S-1-3-D 1))
                        (S-2-1-T (travel (string-append "S-2-1" "-" (number->string S-2-1)) S-2-1-O S-2-1-D 1))
                        (S-2-2-T (travel (string-append "S-2-2" "-" (number->string S-2-2)) S-2-2-O S-2-2-D 1))
                        (S-2-3-T (travel (string-append "S-2-3" "-" (number->string S-2-3)) S-2-3-O S-2-3-D 1))
                    )
                    (cond
                        ((not (equal? (pdata-ref "p" S-2-3) S-2-3-D))
                            (pdata-set! "p" S-1-1 S-1-1-T)
                            (pdata-set! "p" S-1-2 S-1-2-T)
                            (pdata-set! "p" S-1-3 S-1-3-T)
                            (pdata-set! "p" S-2-1 S-2-1-T)
                            (pdata-set! "p" S-2-2 S-2-2-T)
                            (pdata-set! "p" S-2-3 S-2-3-T)
                            (recalc-normals 1)
                        )
                        (else
                            (hash-remove! (hash-ref beat-sphere-square-list id) S-1-1)
                            (hash-remove! (hash-ref beat-sphere-square-list id) S-2-1)
                            #f
                        )
                    )
                )
            )
        )
        (else
            #f
        )
    )
)

(define beat-sphere-square-list (make-hash))

(define (beat-sphere-find-swap id)
    (unless (hash-has-key? beat-sphere-square-list id)
        (hash-set! beat-sphere-square-list id (make-hash))
    )
    (letrec
        (
            (find-swap
                (lambda ()
                    (let
                        (
                            (first-swap
                                (* (quotient (floor (* (flxrnd) (pdata-size))) 3) 3)
                            )
                        )
                        (cond
                            ((hash-has-key? (hash-ref beat-sphere-square-list id) first-swap)
                                (find-swap)
                            )
                            (else
                                (hash-set! (hash-ref beat-sphere-square-list id) first-swap #t)
                                first-swap
                            )
                        )
                    )
                )
            )
        )
        (find-swap)
    )
)

(define (beat-sphere-launcher-swap id)
            (let*
                (
                    (first-swap (inexact->exact (beat-sphere-find-swap id)))
;                    (second-swap (+ first-swap 3))
                    (second-swap (inexact->exact (beat-sphere-find-swap id)))
                    (first-parm (vector (pdata-ref "p" first-swap) (pdata-ref "p" (+ first-swap 1)) (pdata-ref "p" (+ first-swap 2))))
                    (second-parm (vector (pdata-ref "p" second-swap) (pdata-ref "p" (+ second-swap 1)) (pdata-ref "p" (+ second-swap 2))))
                    (function-name (string->symbol (string-append id "-" "swap" "-" (number->string (inexact->exact first-swap)))))
                )
                (spawn-task
                    (lambda ()
                        (beat-sphere-swap
                            id
                            first-swap
                            second-swap
                            first-parm
                            second-parm
                        )
                    )
                    function-name
                )
            )
)

(define (beat-sphere id cross)
    (unless (hash-has-key? beat-sphere-prims id)
        (beat-sphere-build id)
    )
;        (rotate (vmul (vector (* (time-now) 1.987) (* (time-now) 1.0789) (* (time-now) 0.51245)) 0.05))
    (with-primitive (hash-ref beat-sphere-prims id)
        (when (beat-catch id)
            (beat-sphere-launcher-swap id)
            (beat-sphere-launcher-swap id)
            (beat-sphere-launcher-swap id)
        )
        (let ((g (c "gain" id)))
            (pdata-index-map!
                (lambda (i color)
                    (vector (* (gh2 2 g) (c "Rouge" id)) (* (gh2 (* i 2) g) (c "Vert" id)) (* (gh2 (* i 4) g) (c "Bleu" id)) 1)
                )
                "c"
            )
        )
    )
)
