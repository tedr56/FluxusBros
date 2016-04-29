(require racket/math)

;(clear)

(define valley-prims (make-hash))

(define (valley-destroy id)
    (when (hash-has-key? valley-prims id)
        (destroy (hash-ref valley-prims id))
        (hash-remove! valley-prims id)
    )
    (hash-remove! master-time-hash id)
    (hash-remove! slave-time-hash  id)

)
;(define line (build-seg-plane 20 10))

#(define (cylindric->cartesien c)
;(show c)
    (let
        (
            (p (vector-ref c 0))
            (g (vector-ref c 1))
            (z (vector-ref c 2))
        )
        (vector
            (* p (cos (degre->pi g)))
            (* p (sin (degre->pi g)))
            z
        )
    )
)

#(define (cartesien->cylindric c)
    (let*
        (
            (x (vector-ref c 0))
            (y (vector-ref c 1))
            (z (vector-ref c 2))

            (p (sqrt (+ (sqr x) (sqr y))))
            (g (acos (/ x p)))
        )
        (vector p g z)
    )
)

(define (corner-select i)
    (let ((square-math (- (remainder i 4) 1.5)))
        (vector
            (- 1.5 (abs square-math))
            (/ (+ (sgn square-math) 1) 2)
            1
        )
    )
)

(define (degre->pi a)
    (* a (/ (* 2 pi) 360))
)

(define master-time-hash (make-hash))
(define slave-time-hash  (make-hash))

(define (valley-build id)
    (hash-set! valley-prims id (build-seg-plane 20 10))
    (with-primitive (hash-ref valley-prims id)
    ;    (backfacecull 1)
    ;    (hint-none)
    ;    (hint-unlit)
        (hint-wire)
        (line-width 2)
        (hint-vertcols)
        (pdata-add "master" "f")
        (pdata-add "slave" "f")
        #(pdata-index-map!
            (lambda (i c)
    ;            (vector 1 1 1 1)
                (corner-select i)
            )
            "c"
        )
        (pdata-set! "c" 0 (vector 0 1 0))
        (pdata-set! "c" 1 (vector 1 0 0))
        (pdata-set! "c" 2 (vector 1 0 0))
        (pdata-set! "c" 3 (vector 1 0 0))
        (shinyness 20)
        (recalc-normals 1)
    )
    (hash-set! master-time-hash id (time-now))
    (hash-set! slave-time-hash  id (time-now))
)

;(set-gain! 0.1)
;(smoothing-bias 0.4)
;(blur 0.3)



(define (valley id cross)
    (blur (c "blur" id))
    (set-gain! (c "gain" id))
    (flxseed (inexact->exact (floor 1.2)))
    (letrec
        (
            (master-time (hash-ref master-time-hash id))
            (slave-time  (hash-ref slave-time-hash  id))
            (ncube (inexact->exact (floor (max 3 (c "ncube" id #:coeff 127)))))
            (diametre (c "diametre" id #:coeff 10))
            (coeff-d-gl (c "coeff-d-gh" id #:coeff 10))
            (coeff-color (c "coeff-color" id #:coeff 2))
            (coeff-color1 (c "coeff-color1" id #:coeff 2))
            (coeff-color2 (c "coeff-color2" id #:coeff 2))
            (coeff-color3 (c "coeff-color3" id #:coeff 2))
            (alpha (- 360 (c "alpha" id #:coeff 360)))
            (beta  (/ (- 360 alpha) 2))
            (gamma (/ alpha ncube))
            (master-speed (* .01 (c "master-speed" id #:coeff 10)))
            (slave-speed  (* .0001 (c "slave-speed" id #:coeff 1000)))


            (trans -0.0060)
            (clamp-out 0.5)
            (clamp-in -0.5)

            (neighboor-find-x
                (lambda (i x)
                    (let
                        (
                            (result 0)
                        )
                        (cond
                            ((zero? (vector-ref (corner-select i) 0))
                                (cond
                                    ((zero? (vector-ref (corner-select i) 1))
                                        (set! result (+ (- i (* 4 ncube)) 1))
                                    )
                                    (else
                                        (set! result (+ (- i (* 4 ncube)) -1))
                                    )
                                )
                            )
                            (else
                                (cond
                                    ((zero? (vector-ref (corner-select i) 1))
                                        (set! result (- i 1))
                                    )
                                    (else
                                        (set! result (+ i 1))
                                    )
                                )
                            )
                        )
                        result
                    )
                )
            )
            (follow
                (lambda (n)
                    (cond
                        ((and (zero? (quotient n (* 4 ncube))) (zero? (vector-ref (corner-select n) 0)))
                            (cond
                                ((zero? (vector-ref (corner-select n) 1))
                                    (pdata-set! "master" n (- diametre (* coeff-d-gl (gl (* n (/ 16 (flxrnd)))))))
                                    (pdata-set! "c" n (vmul (vector (gl n coeff-color1) (gl (* 2 n) coeff-color2) (gl (* 4 n) coeff-color3)) coeff-color))
                                    (pdata-get "master" n)
                                )
                                (else
                                    (pdata-set! "master" n (pdata-get "master" (+ n 1)))
                                    (pdata-set! "c" n (vmul (vector (gl n coeff-color1) (gl (* 2 n) coeff-color2) (gl (* 4 n) coeff-color3)) coeff-color))
                                    (pdata-get "master" n)
                                )
                            )
                        )
                        ((and (zero? (quotient n (* 4 ncube))) (= (vector-ref (corner-select n) 0) 1))
                            (pdata-set! "master" n (pdata-get "master" (neighboor-find-x n -1)))
                            (pdata-set! "c" n (vmul (vector (gl n coeff-color1) (gl (* 2 n) coeff-color2) (gl (* 4 n) coeff-color3)) coeff-color))
                            (pdata-get "slave" n)
                        )
                        (else
;                            1
                            (pdata-get "slave" n)
;                            (pdata-get "master" n)
                        )
                    )
                )
            )
            (pdata-decal
                (lambda (type num)
                    (let ((n (- num 1)))
                        (cond
                            ((not (zero? (quotient n (* 4 ncube))))
                                (pdata-set! type n (pdata-get type (neighboor-find-x n -1)))
                            )
                        )
                        (unless (zero? num)
                            (pdata-decal type (sub1 num))
                        )
                    )
                )
            )
        )

        (with-primitive (hash-ref valley-prims id)
            (pdata-index-map!
                (lambda (i p)
                    (cylindric->cartesien
                        (vector
                            (follow i)
                            (+ beta (* (vector-ref (corner-select i) 1) gamma) (* gamma (remainder (quotient i 4) ncube)))
                            (+ (vector-ref (corner-select i) 0) (quotient i (* 4 ncube)))
                        )
                    )
                )
                "p"
            )

            (when (> (time-now) (+ master-speed master-time))
                (pdata-decal "master" (pdata-size))
                (set! master-time (time-now))
            )
            (when (> (time-now) (+ slave-speed slave-time))
                (pdata-decal "c" (pdata-size))
                (pdata-index-map!
                    (lambda (i s)
;(show (abs (- s (* 0.9 (pdata-get "master" i)))))
(- diametre (* (- (exp (* (abs (- (pdata-get "master" i) s)) 0.61)) 1) 1))
;(- 2 (* 0.5 (* (abs (- (pdata-get "master" i) s)) 0.61)) 1)
(pdata-get "master" i)
                    )
                    "slave"
                )
                (set! slave-time (time-now))
            )
            (recalc-normals 1)
        )
    )
)
