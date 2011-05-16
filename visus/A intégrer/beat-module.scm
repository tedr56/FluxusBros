(module beat-module racket
    (require fluxus-017/fluxus-midi)
    (require "vjbros.scm")
    (provide
        beat-catch
    )
    (define beat-memory (make-hash))
    (define (beat-catch id #:name (name "0") #:first-beat (first-beat 0) #:beat (beat-pattern (list 1)) #:mode-continuous (continuous-mode #t) #:beat-limit (beat-limit 4))
        (let*
            (
                (id-name (get-beat-name id name))
                (memory (hash-ref! beat-memory id-name 0))
            )
            (cond
                ((< memory (- (length beat-pattern) 1))
                    (cond
                        ((>=
                            (+ (vector-ref (midi-position) 1) (/ (vector-ref (midi-position) 2) 24))
                            (+
                                first-beat
                                (foldl
                                    +
                                    0
                                    (list-tail
                                        (reverse
                                            beat-pattern
                                        )
                                        memory
                                    )
                                )
                            )
                        )
                        (hash-set! beat-memory id-name (+ memory 1))
                        (when (>= (hash-ref beat-memory id-name 0) beat-limit)
                            (hash-set! beat-memory id-name 0)
                        )
                        #t
                        )
                        (else #f)
                    )
                )
                (else
;(show "")
;(show "debug length pattern")
;(display "memory   : ")(show memory)
;(display "beat     : ")(show (+ (vector-ref (midi-position) 1) (/ (vector-ref (midi-position) 2) 24)))
;(display "beat-num : ")(show (+ first-beat (foldl + 0 beat-pattern) (* (- memory (- (length beat-pattern) 1))(list-ref beat-pattern (- (length beat-pattern) 1)))))
                    (cond
                        ((and
                            continuous-mode
                            (>=
                                (+ (vector-ref (midi-position) 1) (/ (vector-ref (midi-position) 2) 24))
                                (+
                                    first-beat
                                    (foldl
                                        +
                                        0
                                        beat-pattern
                                    )
                                    (*
                                        (-
                                            (- memory 1)
                                            (- (length beat-pattern) 1)
                                        )
                                        (list-ref beat-pattern (- (length beat-pattern) 1))
                                    )
                                )
                            )
                        )
                        (hash-set! beat-memory id-name (+ memory 1))
                        (when (>= (hash-ref beat-memory id-name 0) beat-limit)
                            (hash-set! beat-memory id-name 0)
                        )

                        #t
                        )
                        (else #f)
                    )
                )
            )
        )
    )
    (define (get-beat-name id name)
        (string-append id "-" name)
    )
)
