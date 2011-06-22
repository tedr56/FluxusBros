(module beat-module racket
    (require fluxus-018/fluxus-midi)
    (require fluxus-018/fluxus)
    (require "vjbros.scm")
    (require racket/vector)
    (require racket/list)
    (provide
        beat-catch
    )
    (define beat-memory (make-hash))
    (define (beat-catch id #:name (name "0") #:first-beat (first-beat 0) #:beat (beat-pattern (list 1)) #:mode-continuous (continuous-mode #t) #:beat-limit (beat-limit 4))
        (let
            (
                (mid-pos (midi-position))
            )
            (letrec
                (
                    (set-first-beat
                        (lambda (Id (s 0))
                            (let
                                (
                                    (beat-sum (get-beat-sum s))
                                    (pos-sum (get-pos-sum))
                                )
                                (cond
                                    ((< beat-sum  pos-sum)
                                        (set-first-beat Id (+ s 1))

;                                        (list s (get-beat-vector beat-sum) (get-beat-vector (get-beat-sum (+ s 1))))
                                    )
                                    ((>= beat-sum  pos-sum)
                                        (list s (add-beat mid-pos beat-sum) (add-beat mid-pos (get-beat-sum (+ s 1))))
                                    )
                                )
                            )
                        )
                    )
                    (set-next-beat
                        (lambda (I)
                            (hash-set! beat-memory I (list (+ (first (hash-ref beat-memory I)) 1) (second (hash-ref beat-memory I)) (add-beat (get-first-beat I) (get-beat-sum (+ (first (hash-ref beat-memory I)) 1)))))
                            (cond
                                ((equal? (vector #t #t #t) (vector-map >= (get-next-beat I) (add-beat (get-first-beat I) beat-limit)))
                                    (hash-set! beat-memory I (list 0 (add-beat (get-first-beat I) beat-limit) (add-beat (get-first-beat I) (get-beat-sum (+ (first (hash-ref beat-memory I)) 0)))))
                                )
                            )
                            #t
                        )
                    )
                    (add-beat
                        (lambda (v l)
                            (let*
                                (
                                    (bar-base (vector-ref v 0))
                                    (beat-base (vector-ref v 1))
                                    (tick-sup (- (floor l) l))
                                    (beat-sup (remainder (+ l beat-base tick-sup) (midi-beats-per-bar)))
                                    (bar-sup (+ bar-base (quotient (+ l beat-base tick-sup) (midi-beats-per-bar))))
                                    (result (vector bar-sup beat-sup (* tick-sup (midi-clocks-per-beat))))
                                )
                                result
                            )
                        )
                    )
                    (get-first-beat
                        (lambda (i)
                            (second (hash-ref beat-memory i))
                        )
                    )
                    (get-mem-beat
                        (lambda (iD)
                            (first (hash-ref beat-memory iD))
                        )
                    )
                    (get-beat-vector
                        (lambda (S)
                            (vector (vector-ref mid-pos 0) (floor S) (* (midi-clocks-per-beat) (- S (floor S))))
                        )
                    )
                    (get-pos-sum
                        (lambda ()
                            (+ (vector-ref mid-pos 1) (/ (vector-ref mid-pos 2) (midi-clocks-per-beat)))
                        )
                    )
                    (get-beat-sum
                        (lambda (n)
                            (+
                                first-beat
                                (get-pattern-sum n)
                            )
                        )
                    )
                    (get-pattern-sum
                        (lambda (N)
                            (cond
                                ((not continuous-mode)
                                    (foldl
                                        +
                                        0
                                        (take beat-pattern (min N (length beat-pattern)))
                                    )
                                )
                                (continuous-mode
                                    (+
                                        (foldl
                                            +
                                            0
                                            (take beat-pattern (min N (length beat-pattern)))
                                        )
                                        (*
                                            (last beat-pattern)
                                            (max
                                                (-
                                                    N
                                                    (length beat-pattern)
                                                )
                                                0
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (get-next-beat
                        (lambda (i)
                            (third (hash-ref beat-memory i))
                        )
                    )
                    (get-id-name
                        (lambda (ID NAME)
                            (string-append ID "-" NAME)
                        )
                    )
                )
                (let ((identifiant (get-id-name id name)))
                    (unless (hash-has-key? beat-memory identifiant)
                        (hash-set! beat-memory identifiant (set-first-beat identifiant))
                    )
                    (cond
                        ((equal? (vector #t #t #t) (vector-map >= mid-pos (get-next-beat identifiant)))
                            (set-next-beat identifiant)
                            #t
                        )
                        (else
                            #f
                        )
                    )
                )
            )
        )
    )
)
