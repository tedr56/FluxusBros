(define current-beat (vector-ref (midi-position) 1))
(define (next-beat)
    (let ((new-beat (vector-ref (midi-position) 1)))
        (cond
            ((not (= new-beat current-beat))
                (set! current-beat new-beat)
                #t
            )
            (else
            #f
            )
        )
    )
)

(ground-plane (vector 0 1 0) -1)
(collisions 1)

(define gogol-prims (make-hash))
(define gogol-positions (make-hash))

(define (gogol-translate)
    (letrec
        (
            (pos-rnd
                (lambda ()
                    (vector (floor (- (* 10 (flxrnd)) 5)) 0 (floor (- (* 20 (flxrnd)) 10)))
                )
            )
            (pos-free?
                (lambda (pos)
                    (let ((pos-free-var pos))
                        (hash-for-each
                            gogol-positions
                            (lambda (k v)
                                (when
                                    (and
                                        (and
                                            (> (vector-ref pos 0) (- (vector-ref v 0) 3))
                                            (< (vector-ref pos 0) (+ (vector-ref v 0) 3))
                                        )
                                        (and
                                            (> (vector-ref pos 1) (- (vector-ref v 1) 3))
                                            (< (vector-ref pos 1) (+ (vector-ref v 1) 3))
                                        )
                                    )
                                    (set! pos-free-var #f)
                                )
                            )
                        )
                        pos-free-var
                    )
                )
            )
            (pos-search
                (lambda (position)
                    (let*
                        ((pos-result (pos-free? (pos-rnd))))
                        (cond
                            (pos-result
;(display "debug pos-result :")(show pos-result)
                                pos-result
                            )
                            (else
;(display "debug pos-result wrong :")(show pos-result)
                                (pos-search (pos-rnd))
                            )
                        )
                    )
                )
            )
        )
        (pos-search (pos-rnd))
    )
)

(define (gogol-add-prim id name value)
    (unless (hash-has-key? gogol-prims id)
        (hash-set! gogol-prims id (make-hash))
    )
    (hash-set!
        (hash-ref
            gogol-prims
            id
        )
        name
        value
    )
)

(define (gogol-get-prims name id)
    (hash-ref
        (hash-ref
            gogol-prims
            id
        )
        name
    )
)

(define (gogol-build id)
    (let*
        (
            (character-translate (gogol-translate))
            (shape1
                (with-state
                    (translate (vector 0 6 0))
                    (translate character-translate)
                    (build-sphere 10 10)))
            (shape2
                (with-state
                    (translate (vector 0 4 0))
                    (translate character-translate)
                    (scale (vector 1.5 1 1))
                    (build-cube)))

            (shape3
                (with-state
                    (translate (vector 0 2 0))
                    (translate character-translate)
                    (build-cube)))
            (shape4
                (with-state
                    (translate (vector -1 0 0))
                    (translate character-translate)
                    (scale (vector 1.5 1 1.5))
                    (build-cube)))
            (shape4.1
                (with-state
                    (translate (vector 1 0 0))
                    (translate character-translate)
                    (scale (vector 1.5 1 1.5))
                    (build-cube)))
            (shape2.5
                (with-state
                    (translate (vector -2 4 0))
                    (translate character-translate)
                    (build-cube)))
            (shape2.6
                (with-state
                    (translate (vector 2 4 0))
                    (translate character-translate)
                    (build-cube)))
        )
(show "debug gogol let")
        (gogol-add-prim id "shape1" shape1)
        (gogol-add-prim id "shape2" shape2)
        (gogol-add-prim id "shape2.5" shape2.5)
        (gogol-add-prim id "shape2.6" shape2.6)
        (gogol-add-prim id "shape3" shape3)
        (gogol-add-prim id "shape4" shape4)
        (gogol-add-prim id "shape4.1" shape4.1)
(show "debug gogol add-prims")
        (active-box (gogol-get-prims "shape1" id))
        (active-box (gogol-get-prims "shape2" id))
        (active-box (gogol-get-prims "shape2.5" id))
        (active-box (gogol-get-prims "shape2.6" id))
        (active-box (gogol-get-prims "shape3" id))
        (active-box (gogol-get-prims "shape4" id))
        (active-box (gogol-get-prims "shape4.1" id))
(show "debug gogol active-box")
        (set-mass (gogol-get-prims "shape4" id) 50)
        (set-mass (gogol-get-prims "shape4.1" id) 50)
(show "debug gogol set-mass")
(show character-translate)
        (let
            (
                (bj1
                    (build-balljoint (gogol-get-prims "shape1" id) (gogol-get-prims "shape2" id) (vadd (vector 0 5 0) character-translate))
                )
                (bj2
                    (build-balljoint (gogol-get-prims "shape2" id) (gogol-get-prims "shape3" id) (vadd (vector 0 3 0) character-translate))
                )
                (bj3
                    (build-balljoint (gogol-get-prims "shape3" id) (gogol-get-prims "shape4" id) (vadd (vector -1 1 0) character-translate))
                )
                (bj4
                    (build-balljoint (gogol-get-prims "shape3" id) (gogol-get-prims "shape4.1" id) (vadd (vector .5 1 0) character-translate))
                )
                (bj5
                    (build-balljoint (gogol-get-prims "shape2" id) (gogol-get-prims "shape2.5" id) (vadd (vector -.5 4 0) character-translate))
                )
                (bj6
                    (build-balljoint (gogol-get-prims "shape2" id) (gogol-get-prims "shape2.6" id) (vadd (vector 1 4 0) character-translate))
                )
            )
(show "debug gogol let build-balljoint")
            (gogol-add-prim id "bj1" bj1)
            (gogol-add-prim id "bj2" bj2)
            (gogol-add-prim id "bj3" bj3)
            (gogol-add-prim id "bj4" bj4)
            (gogol-add-prim id "bj5" bj5)
            (gogol-add-prim id "bj6" bj6)
        )
        (hash-set! gogol-positions id character-translate)
        (set-physics-debug #f)
(show "debug gogol-build-print-scene-graph")
(print-scene-graph)
    )
)

(define (gogol-destroy id)
    (hash-for-each
        (hash-ref
            gogol-prims
            id
        )
        (lambda (k v)
            (destroy v)
        )
    )
    (hash-remove! gogol-prims id)
    (show gogol-prims)
(print-scene-graph)
)

(define (gogol id cross)
    (unless (hash-has-key? gogol-prims id)
(show "debug gogol build-------------------")
        (gogol-build id)
    )
    (with-primitive (gogol-get-prims "shape1" id) (colour (vector (gh 2) (gh 3) (gh 4))))
    (when (next-beat)
        (kick (gogol-get-prims "shape1" id) (vector 0 15 0))
        (kick (gogol-get-prims "shape2" id) (vector 0 2 0))
        (kick (gogol-get-prims "shape3" id) (vector 0 -7 0))
        (kick (gogol-get-prims "shape4" id) (vector 0 .5 0))
        (kick (gogol-get-prims "shape2.5" id) (vmul (vector 2 0 -2) (gh 12)))
        (kick (gogol-get-prims "shape2.6" id) (vmul (vector -2 0 2) (gh 12)))
        (twist (gogol-get-prims "shape2" id) (vector 0 (flxrnd) 0))

        (with-primitive (gogol-get-prims "shape1" id) (colour (vector (gh 2) (gh 2) 0)))
        (with-primitive (gogol-get-prims "shape2" id) (colour (vector 0 (gh 6) (gh 10))))
        (with-primitive (gogol-get-prims "shape3" id) (colour (vector 0 (gh 9) .1)))
        (with-primitive (gogol-get-prims "shape4" id) (colour (vector (gh 12) 0 (gh 4))))
        (with-primitive (gogol-get-prims "shape4.1" id) (colour (vector (gh 12) 0 0)))
        ;(with-primitive (gogol-get-prims "shape2.5" id) (colour (vector (gh 11) (gh 12) (gh 13))))
        ;(with-primitive (gogol-get-prims "shape2.6" id) (colour (vector (gh 11) (gh 12) (gh 13))))
        (with-primitive (gogol-get-prims "shape2.5" id) (colour 1))
        (with-primitive (gogol-get-prims "shape2.6" id) (colour 1))
    )
)
;(every-frame (test))
