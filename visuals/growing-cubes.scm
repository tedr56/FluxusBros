(define (growing-cubes-destroy id)
    (when (hash-has-key? growing-cubes-prims id)
        (for-each
            (lambda (l)
                (let*
                    (
                        (Name (send id get-name))
                        (prim (car l))
                        (travel-function (string->symbol (string-append "growing-cubes" "-" Name "-" (number->string prim))))
                    )
                    (when (task-running? travel-function)
                        (travel-end (string-append "growing-cubes" "-" Name "-" (number->string prim)))
                        (rm-task travel-function)
                    )
                    (destroy prim)
                )
            )
            (hash-ref growing-cubes-prims id)
        )
        (hash-remove! growing-cubes-prims id)
    )
)

(define growing-cubes-prims (make-hash))
(define (growing-cubes id cross)
    (letrec
        (
            (Name (send id get-name))
            (g (c "gain" id))
            (control-scale (c "scale" id))
            (control-colour (c "colour" id #:coeff 3))
            (add-cube
                (lambda ()
                    (let
                        (
                            (new-cube
                                (with-state
;                                    (when (positive? (length (hash-ref growing-cubes-prims id)))
;                                        (parent
;                                            (car
;                                                (list-ref
;                                                    (hash-ref growing-cubes-prims id)
;                                                    (- (length (hash-ref growing-cubes-prims id)) 1)
;                                                )
;                                            )
;                                        )
;                                    )
                                    (build-cube)
                                )
                            )
                        )
                        (hash-set! growing-cubes-prims id (append (hash-ref growing-cubes-prims id) (list (cons new-cube (vector 0 0 0)))))
                        (parent new-cube)
                        (with-primitive new-cube
;                            (hint-none)
                            (hint-wire)
                            (line-width 2)
;                            (hide 1)
                        )
                    )
                    (when (> (length (hash-ref growing-cubes-prims id)) 100)
                        (del-cube)
                    )
                )
            )
            (find-direction
                (lambda (n)
                    (let*
                        (
                            (axis (inexact->exact (floor (* (flxrnd) 3))))
                            (sens (- (* (floor (* (flxrnd) 2)) 2) 1))
                            (direction (vector 0 0 0))
                        )
                        (vector-set! direction axis sens)
                        (letrec
                            (
                                (check-direction
                                    (lambda (i)
                                        (cond
                                            ((equal? (vector 0 0 0) (vadd direction (cdr (list-ref (hash-ref growing-cubes-prims id) i))))
                                                #f
                                            )
                                            (else
                                                (cond
                                                    ((> i 0)
                                                        (check-direction (- i 1))
                                                    )
                                                    (else
                                                        #t
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                            (cond
                                ((>= (length (hash-ref growing-cubes-prims id)) 2)
                            (cond
                                ((check-direction (- (length (hash-ref growing-cubes-prims id)) 2))
                                    direction
                                )
                                (else
                                    (cond
                                        ((> n 0)
                                            (find-direction (- n 1))
                                        )
                                        (else
                                            direction
                                        )
                                    )
                                )
                            )
                                )
                                (else
                                    (vector 0 0 0)
                                )
                            )
                        )
                    )
                )
            )
            (travel-cube
                (lambda (prim origine destination direction)
                    (spawn-task
                        (lambda ()
                            (let
                                (
                                    (voyage
                                        (travel
                                            (string-append "growing-cubes" "-" Name "-" (number->string prim))
;                                            origine
;                                            destination
                                            (vector 0 0 0)
                                            direction
                                            0.2
                                        )
                                    )
                                )
                                (cond
                                    ((equal? voyage destination)
                                        (travel-end (string-append "growing-cubes" "-" Name "-" (number->string prim)))
                                        #f
                                    )
                                    (else
                                        (with-primitive prim
                                            (identity)
                                            (rotate (vmul voyage 90))
                                            (translate (vadd origine voyage))
;// SCALE (optional)
                                            (scale (vmul (vector (min (gl prim g) 2) (min (gl prim g) 2) (min (gl prim g) 2)) (* 0.5 control-scale)))
                                            (colour (vmul (vector (gl prim g) (gl (+ 3 prim) g) (gl (+ prim 6) g)) control-colour))
                                            (wire-colour (vmul (vector (gl prim g) (gl (+ 2 prim) g) (gl (- prim 2) g)) control-colour))
                                        )
                                    )
                                )
                            )
                        )
                        (string->symbol (string-append "growing-cubes" "-" Name "-" (number->string prim)))
                    )
                )
            )
            (move-cube
                (lambda ()
                    (let ((dir (find-direction 10)))
                        (hash-set!
                            growing-cubes-prims
                            id
                            (map
                                (lambda (l)
                                    (cond
                                        ((= (car l) (car (list-ref (hash-ref growing-cubes-prims id) (- (length (hash-ref growing-cubes-prims id)) 1))))
                                            l
                                        )
                                        (else

;                                        ((= (car l) (car (list-ref (hash-ref growing-cubes-prims id) (- (length (hash-ref growing-cubes-prims id)) 2))))
;)
                                            (travel-end (string-append "growing-cubes" "-" Name "-" (number->string (car l))))
                                            (travel-cube (car l) (cdr l) (vadd dir (cdr l)) dir)
                                            (cons (car l) (vadd dir (cdr l)))
                                        )
;                                        (else
;                                            l
;                                        )
                                    )
                                )
                                (hash-ref growing-cubes-prims id)
                            )
                        )
                    )
                )
            )
            (del-cube
                (lambda ()
                    (let*
                        (
                            (list-prims (hash-ref growing-cubes-prims id))
                            (prim (car (list-ref list-prims 0)))
                            (travel-function (string->symbol (string-append "growing-cubes" "-" Name "-" (number->string prim))))
                        )
                        (when (task-running? travel-function)
                            (travel-end (string-append "growing-cubes" "-" Name "-" (number->string prim)))
                            (rm-task travel-function)
                        )
                        (destroy prim)
                        (hash-set! growing-cubes-prims id (list-tail (hash-ref growing-cubes-prims id) 1))
                    )
                )
            )
        )
        (unless (hash-has-key? growing-cubes-prims id)
            (hash-set! growing-cubes-prims id '())
        )
        (flxseed (+ (inexact->exact (floor (* 1000 (flxrnd) (time-now))))))
        (when (beat-catch (send id get-name) "addcube")
            (add-cube)
            (move-cube)
        )
        (for-each
            (lambda (l)
                (let ((prim (car l)))

                    (with-primitive prim
                        (colour (vmul (vector (gl prim g) (gl (+ 3 prim) g) (gl (+ prim 6) g)) .00000))
                        (wire-colour (vmul (vector (gl prim g) (gl (+ 2 prim) g) (gl (- prim 2) g)) 0))
                    )
                )
            )
            (hash-ref growing-cubes-prims id)
        )
    )
)
    
