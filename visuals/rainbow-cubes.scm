;; hackpact day 13 - cubes
;; gabor papp http://mndl.hu/hackpact

;(clear)

;(hint-solid)
;(backfacecull 0)

(define (invert-point p cp r)
    (vadd
        cp
        (vmul
            (vsub p cp)
            (/ (* r r)
               (vdist-sq p cp)))))

(define rainbow-cubes-prims (make-hash))

(define (rainbow-cubes-build id)
    (let
        (
            (cubes
                (for*/list
                    (
                        [x (in-range -2 2)]
                        [y (in-range -2 2)]
                        [z (in-range -2 2)])
                    (with-state
                        (hint-wire)
                        (hint-ignore-depth)
                        (hint-depth-sort)

                        (translate (vector x y z))
                        (scale .7)
                        (build-cube)))
            )
        )
        (for-each
          (lambda (p)
            (with-primitive p
                (apply-transform)
                (pdata-copy "p" "p0")))
          cubes)
        (hash-set! rainbow-cubes-prims id cubes)
    )
)

(define (rainbow-cubes-destroy id)
    (for-each
        (lambda (p)
            (destroy p)
            
        )
        (hash-ref rainbow-cubes-prims id)
    )
    (hash-remove! rainbow-cubes-prims id)
)

(define (rainbow-cubes id cross)
    (let*
        (
            [g (* (c "gain-a" id) (c "gain-b" id))]
            [blur-control (c "blur" id)]
            [opacity-control (c "opacity" id)]
            [color-control (c "color" id)]
            [coeff-speed (c "speed" id)]
            [tempo-Rx (mn 0 34 coeff-speed 'tempo-x)]
            [tempo-Ry (mn 0 35 coeff-speed 'tempo-y)]
            [tempo-Rz (mn 0 36 coeff-speed 'tempo-z)]
            [invert-x (c "invert-x" id)]
            [invert-y (c "invert-y" id)]
            [invert-z (c "invert-z" id)]
            [invert-gain-base (c "invert-gain-base" id #:coeff 3)]
            [invert-gain-gh (c "invert-gain-gh" id)]
            [invert-radius-base (c "invert-radius-base" id)]
            [invert-radius-gh (c "invert-radius-gh" id)]
            [a2 (* 0.233 (time-now))]
            [a (* 0.233 (tempo "R" (+ (mn 0 37 10) (* (mn 0 29 (mn 0 21)) (gl 1)))))]
            [inv-p
                (vector
                    (* 1.8 (sin (* .5435987 a)) (sin a))
                    (* 3.8 (sin (* .821108 a)) (cos a))
                    (* 2.9 (cos a)))]
            [inv-p2
                (vector
                    (sin (tempo "Rx" tempo-Rx))
                    (sin (tempo "Ry" tempo-Ry))
                    (cos (tempo "Rz" tempo-Rz)))]
            [Cubes (hash-ref rainbow-cubes-prims id)]
            [NbCubes (length Cubes)]
        )
        (for
            (
                [c Cubes]
                [i (in-range NbCubes)]
            )
            (with-primitive c
                (opacity (* opacity-control (gl i)))
                (wire-opacity (gh i))
                (let ((color (/ i NbCubes)))
                    (colour (hsv->rgb (vector color 1 (* color-control (gl i)))))
                )
                (pdata-map!
                    (lambda (p p0)
                        (vmul (invert-point p0 (vmul2 (vector invert-x invert-y invert-z) inv-p) (+ invert-gain-base (* invert-gain-gh (gl i)))) (+ invert-radius-base (* invert-radius-gh (gl i))))
                    )
                    "p" "p0")))
        (blur blur-control)
    )
)

;(every-frame (rainbow-cubes 1 1))
