(blur 0)

(collisions 0)
;(set-max-physical 1000)
(hint-wire)
(ground-plane (vector 0 1 0 ) -3)


(define (smokesound id cross)
    (collision 0)
    (let*
        (
            (g (c "gain" id))
            (blur-c (c "blur" id))
            (opa-static (c "opa-static" id))
            (opa-gh (c "opa-gh" id))
            (hint-solid-wire (c "hint-solid-wire" id))
            (line-width-c (c "line-width-c" id))
            (max-prims (c "max-prims" id))
        )
        (with-state

            (cond
                ((= 0 (mn chann 13))
                    (hint-none)
                    (hint-solid))
                    (colour (vmul (vector (* 1 (gh 4)) (* 0.1 (gh 2)) (* 0.1 (gh 3))) (mn chann 14)))
                    (opacity (+ (mn chann 11) (* (gh n) (mn chann 12))))
                (else
                    (hint-wire)
                    (line-width (* (mn chann 13) 10))
                    (wire-colour (vector (* 1 (gh 12)) (* 0.1 (gh 5)) (* 0.1 (gh 2))))
                    (wire-opacity (+ (mn chann 11) (* (gh n) (mn chann 12))))
                )
            )

            (translate (vmul (vector (* -10 (m chann 44)) (* -10 (m chann 45)) (* -10 (m chann 46))) (* (mn chann 37) 10)))
            (translate (vmul (vector (* 2 (gh 1)) 0 (* 2 (gh 6))) (* (* (mn chann 36) 1) (- (* (mn chann 58) 2) 1))))
            (translate (vector (* (- (* (mn chann 58) 2) 1) (* -1 (m chann 38))) 0 0))
            (scale (vmul (vector (* (mn chann 35) (/ (gh (+ n 3)) 2)) (/ (gh n) 2) (* (mn chann 35) (gh 10))) (+ 0.1 (* (mn chann 43) 20))))

            (unless (hash-has-key? smoke-sound-prims id)
                (hash-set! smoke-sound-prims id '())
            )

            (let
                (
                    (ob (build-cube))
                )
                (hash-set! smoke-sound-prims id (append (hash-ref smoke-sound-prims id) ob))
                (when (> (length (hash-ref smoke-sound-prims id)) max-prims)
                    (destroy (list-ref (hash-ref smoke-sound-prims id)))
                    (hash-set! smoke-sound-prims id (take-right (hash-ref smoke-sound-prims id) 1))

(define (smokesound chann cross n trigger)
    (define (ghm chann f i)
        (+ (* (mn chann i) (gh f)) 0))
(set-max-physical (m chann 47))
(gravity (vector 0 0 0))
(set-physics-debug #t)
(render-physics)
(gain (* (mn chann 8) (mn chann 9)))
(blur (mn chann 10))

;        (rotate (vector (* -1 (m chann 36)) (* -1 (m chann 37)) (* -1 (m chann 38))))
    (with-state
(opacity (+ (mn chann 11) (* (gh n) (mn chann 12))))
(cond ((= 0 (mn chann 13))
    (hint-none)
    (hint-solid))
(else
(hint-wire)

(line-width (* (mn chann 13) 10))))
        (colour (vmul (vector (* 1 (gh 4)) (* 0.1 (gh 2)) (* 0.1 (gh 3))) (mn chann 14)))
        (wire-colour (vector (* 1 (gh 12)) (* 0.1 (gh 5)) (* 0.1 (gh 2))))
;        (rotate (vector (gh 1) (gh 1) (gh 1)))

        (translate (vmul (vector (* -10 (m chann 44)) (* -10 (m chann 45)) (* -10 (m chann 46))) (* (mn chann 37) 10)))
        (translate (vmul (vector (* 2 (gh 1)) 0 (* 2 (gh 6))) (* (* (mn chann 36) 1) (- (* (mn chann 58) 2) 1))))
    (translate (vector (* (- (* (mn chann 58) 2) 1) (* -1 (m chann 38))) 0 0))
        (scale (vmul (vector (* (mn chann 35) (/ (gh (+ n 3)) 2)) (/ (gh n) 2) (* (mn chann 35) (gh 10))) (+ 0.1 (* (mn chann 43) 20))))
(opacity (m 0 cross))
(wire-opacity (m 0 cross))

(cond ((= trigger 1)
(let ((ob (build-cube)))
(grab ob) (recalc-normals 0) (ungrab)
            (active-box ob)
            (kick ob (vector (* (* (mn chann 40) 100) (ghm chann n 32)) (* (* (mn chann 41) 100) (ghm chann n 33)) (* (* (mn chann 42) 100) (ghm chann n 34))))
)
)
(else
;(with-primitive ob (hide 1))
(set-max-physical 0)
))


))

;(every-frame (smokesound 3))
;(spawn-task (lambda () (smokesound 3)) 'smokesound)

(define smokesound-state 0)

(define (smokesound-control chann cross)

    (cond ((and (= smokesound-state 0) (> (m 0 cross) 0))
        (set! smokesound-state 1)
;    (create-prim)
;(clear)
        (spawn-task (lambda () (smokesound chann cross 3 1)) 'smokesound)))
    (cond ((and (= (m 0 cross) 0))
        (set! smokesound-state 0)
;
;        (rm-task 'smokesound)
        (spawn-task (lambda () (smokesound chann cross 3 0)) 'smokesound)

        ))
)

;(every-frame (smokesound-control 0 49))
