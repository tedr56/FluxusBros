(require "beat-module.scm")
(require scheme/list)
(define bassparicules-number 100)
(define BassParticules-prims (make-hash))
(define BassParticules-prims-torus (make-hash))
(define BassParticulesTexture (load-texture "splat.png"))
(define (bassparticules-build id)
    (letrec
        (
            (build-bassparticules
                (lambda ()
                    (with-state
                        (hint-ignore-depth)
                        (blend-mode 'src-alpha 'one)
                        (texture BassParticulesTexture)
                        (build-particles bassparicules-number)
                    )
                )
            )
            (build-basstorus
                (lambda ()
                    (let ((prim (build-torus 0.2 1 16 20)))
                        (with-primitive prim
                            (hint-vertcols)
                            (scale (vector 1 0.4 1))
                            (pdata-map!
                                (lambda (c)
                                    (vector 1 0 0 1)
                                )
                                "c"
                            )
                            (rotate (vector 90 0 0))
                            (apply-transform)
                        )
                        prim
                    )
                )
            )
            (set-bassparticules
                (lambda ()
                    (letrec
                        (
                            (BP1 (build-bassparticules))
                            (BP2 (build-bassparticules))
                            (BPvel
                                (lambda (p)
                                    (with-primitive p
                                        (pdata-add "vel" "v")
                                    )
                                    p
                                )
                            )
                        )
                        (hash-set! BassParticules-prims id (list (BPvel BP1) (BPvel BP2)))
                    )
                )
            )
            (set-basstorus
                (lambda ()
                    (hash-set! BassParticules-prims-torus id (list (build-basstorus) (build-basstorus)))
                )
            )
        )
        (set-bassparticules)
        (set-basstorus)
    )
)

(define (bassparticules id cross)
    (letrec
        (
            (translate-c (c "ecart" id #:coeff 10))
            (rotation-c (c "rotate-g" id #:coeff 10))
            (rotation-c1 (c "rotate-1" id #:coeff 3))
            (rotation-c2 (c "rotate-2" id #:coeff 3))
            (size-c (+ 0.2 (c "size" id #:coeff 10)))
            (opa-c (c "blowop" id #:coeff 2))
            (beat-1 (+ 1 (c "beat1" id #:coeff 1)))
            (beat-2 (+ 1 (c "beat2" id #:coeff 1)))
            (blow-c (c "blow" id #:coeff 1))
            (gravity-c (c "gravity" id #:coeff 1))
            (Prims (hash-ref BassParticules-prims id))
            (TPrims (hash-ref BassParticules-prims-torus id))
            (gravity (vector 0 (* -1 gravity-c) 0))
            (time-p (time))
            (rotate-c-l
                (list rotation-c1 rotation-c2)
            )
            (translate-c-l
                (list -1 1)
            )
            (rotate-l
                (lambda (i)
                    (rotate (vmul (vector-of time-p) (* (list-ref rotate-c-l i) rotation-c)))
                )
            )
            (translate-l
                (lambda (i)
                    (translate (vmul (vector translate-c 0 0) (list-ref translate-c-l i)))
                )
            )
            (get-random
                (lambda ()
                    (let ((p-rnd (hsrndvec)))
                        (vector (vector-ref p-rnd 0) 0 (vector-ref p-rnd 2))
                    )
                )
            )
            (set-random
                (lambda (prim sens)
;                    (with-primitive prim
                        (pdata-map!
                            (lambda (p)
                                (get-random)
                            )
                            "p"
                        )
                        (pdata-map!
                            (lambda (vel p)
                                (vadd p (vadd (vmul p blow-c) (vector 0 (* (max 0.5 (* 1 (flxrnd))) sens) 0)))
                            )
                            "vel" "p"
                        )
                        (pdata-map!
                            (lambda (c)
                                (vector 0 -0.05 0 -0.05)
                            )
                            "col"
                        )
                        (pdata-map!
                            (lambda (c)
                                (vector 1 1 0)
                            )
                            "c"
                        )
                        (pdata-map!
                            (lambda (s)
                                size-c
                            )
                            "s"
                        )
;                    )
                )
            )
            (set-particules
                (lambda (n)
                    (cond
                        ((zero? n)
                            (with-primitive (first Prims)
                                (identity)
                                (rotate-l 0)
                                (translate-l 0)
                                (set-random (first Prims) 1)
                            )
                        )
                        (else
                            (with-primitive (second Prims)
                                (identity)
                                (rotate-l 1)
                                (translate-l 1)
                                (set-random (second Prims) -1)
                            )
                        )
                    )
                )
            )
            (set-torus
                (lambda (n)
                    (with-primitive (list-ref TPrims n)
                        (identity)
                        (hide 0)
                        (pdata-map!
                            (lambda (c)
                                (vector 1 0 0 1)
                            )
                            "c"
                        )
                        (rotate-l n)
                        (translate-l n)
                    )
                )
            )
            (blow-bassparticules
                (lambda (prim)
                    (with-primitive prim
                        (pdata-op "*" "vel" 0.8)
                        (pdata-op "+" "p" "vel")
                        (pdata-op "*" "s" 0.9)
                        (pdata-op "+" "c" (vector 0 -0.11 0 (* opa-c -0.011)))
                    )
                )
            )
            (blow-basstorus
                (lambda (prim)
                    (with-primitive prim
                        (scale (vector 1.0851 0.985 1.0851 ))
                        (pdata-op "+" "c" (vector 1 0.1 0 (* opa-c -0.05)))
                        (unless (positive? (vector-ref (pdata-get "c" 0) 3))
                            (hide 1)
                        )
                    )
                )
            )
        )
        (when (beat-catch "1" "beat-1" #:beat (list beat-1))
            (set-particules 0)
            (set-torus 0)
        )
        (when (beat-catch "1" "beat-2" #:first-beat 1 #:beat (list beat-2))
            (set-particules 1)
            (set-torus 1)
        )
        (with-primitive (first Prims)
            (pdata-op "+" "vel" gravity)
        )
        (with-primitive (second Prims)
            (let ((gravity+ (vmul gravity -1)))
                (pdata-op "+" "vel" gravity+)
            )
        )
        (for ((p Prims))
            (blow-bassparticules p)
        )
        (for ((p TPrims))
            (blow-basstorus p)
        )

    )
)

