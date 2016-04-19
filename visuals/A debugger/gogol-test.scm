;(clear)

(require "beat-module.scm")

(ground-plane (vector 0 1 0) -1)
(collisions 1)

(define gogol-prims (make-hash))
(define gogol-joints (make-hash))
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
                                pos-result
                            )
                            (else
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
(display "add-joints ")
(display name)
(display " ")
(show value)
    (hash-set!
        (hash-ref
            gogol-prims
            id
        )
        name
        value
    )
)

(define (gogol-add-joint id name value)
    (unless (hash-has-key? gogol-joints id)
        (hash-set! gogol-joints id (make-hash))
    )
(display "add-joints ")
(display name)
(display " ")
(show value)
    (hash-set!
        (hash-ref
            gogol-joints
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
(show "gogol build")
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

        (gogol-add-prim id "shape1" shape1)
        (gogol-add-prim id "shape2" shape2)
        (gogol-add-prim id "shape2.5" shape2.5)
        (gogol-add-prim id "shape2.6" shape2.6)
        (gogol-add-prim id "shape3" shape3)
        (gogol-add-prim id "shape4" shape4)
        (gogol-add-prim id "shape4.1" shape4.1)

        (active-box (gogol-get-prims "shape1" id))
        (active-box (gogol-get-prims "shape2" id))
        (active-box (gogol-get-prims "shape2.5" id))
        (active-box (gogol-get-prims "shape2.6" id))
        (active-box (gogol-get-prims "shape3" id))
        (active-box (gogol-get-prims "shape4" id))
        (active-box (gogol-get-prims "shape4.1" id))

        (set-mass (gogol-get-prims "shape4" id) 50)
        (set-mass (gogol-get-prims "shape4.1" id) 50)
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
            (gogol-add-joint id "bj1" bj1)
            (gogol-add-joint id "bj2" bj2)
            (gogol-add-joint id "bj3" bj3)
            (gogol-add-joint id "bj4" bj4)
            (gogol-add-joint id "bj5" bj5)
            (gogol-add-joint id "bj6" bj6)
        )
        (hash-set! gogol-positions id character-translate)
        (set-physics-debug #f)
    )
)

(define (gogol-test-destroy id)
    (hash-for-each
        (hash-ref
            gogol-prims
            id
        )
        (lambda (k v)
            (physics-remove v)
        )
    )
    #(hash-for-each
        (hash-ref
            gogol-joints
            id
        )
        (lambda (k v)
            (physics-remove v)
        )
    )
    (hash-for-each
        (hash-ref
            gogol-prims
            id
        )
        (lambda (k v)
            (destroy v)
        )
    )
    #(hash-for-each
        (hash-ref
            gogol-joints
            id
        )
        (lambda (k v)
            (destroy v)
        )
    )
    (hash-remove! gogol-prims id)
    (hash-remove! gogol-joints id)
)

(define (gogol-test id cross)
    (unless (hash-has-key? gogol-prims id)
        (gogol-build id)
    )
    (with-primitive (gogol-get-prims "shape1" id) (colour (vector (gh 2) (gh 3) (gh 4))))
    (when (beat-catch id)
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
        (with-primitive (gogol-get-prims "shape2.5" id) (colour 1))
        (with-primitive (gogol-get-prims "shape2.6" id) (colour 1))
    )
)

#(define (gogol-launch)
    (cond
        ((and (key-pressed "g") (not (task-running? 'gogol1)))
(display "key : ")(show (key-pressed "g"))
            (show "gogol1 launch")
            (spawn-task (lambda () (gogol-test "1" 1)) 'gogol1)
        )
        ((and (not (key-pressed "g")) (task-running? 'gogol1))
            (show "gogol1 kill")
            (rm-task 'gogol1)
            (show "gogol1 removed")
            (show (ls-tasks))
            (gogol-test-destroy "1")
            (show "gogol1 destroyed")
        )
        ((and (key-pressed "h") (not (task-running? 'gogol2)))
            (spawn-task (lambda () (gogol-test "2" 1)) 'gogol2)
        )
        ((and (not (key-pressed "h")) (task-running? 'gogol2))
            (rm-task 'gogol2)
            (gogol-test-destroy "2")
        )
    )
)

;(every-frame (gogol-launch))
