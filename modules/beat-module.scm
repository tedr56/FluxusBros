(module beat-module racket
    (require fluxus-018/fluxus-midi)
    (require fluxus-018/fluxus)
    (require "module-vjbros/vjbros.rkt")
    (require racket/vector)
    (require racket/list)
    (provide
        beat-add
        beat-sub
        beat-translate
        beat-equal?
        beat-catch
    )
; hash
;  id
;   next beat
;   pointeur de beat
;   first-beat
;   last-beat
; hash
;  id-pattern
;   list de beat
;   list de sauvegarde


    (define beat-memory (make-hash))
    (define beat-pattern-memory (make-hash))
    (define (beat-add beat1 beat2)  ; #(bar beat tick)
        (let*
            (
                (tick (inexact->exact (floor (+ (vector-ref beat1 2) (vector-ref beat2 2)))))
                (tick-n (remainder tick (midi-clocks-per-beat)))
                (beat (+ (vector-ref beat1 1) (vector-ref beat2 1) (quotient tick (midi-clocks-per-beat))))
                (beat-n (remainder beat (midi-beats-per-bar)))
                (bar-n (+ (vector-ref beat1 0) (vector-ref beat2 0) (quotient beat (midi-beats-per-bar))))
            )
            (vector bar-n beat-n tick-n)
        )
    )
    (define (beat-sub beat1 beat2)
        (let*
            (
                (t1 (vector-ref beat1 2))
                (t2 (vector-ref beat2 2))
                (b1 (vector-ref beat1 1))
                (b2 (vector-ref beat2 1))
                (B1 (vector-ref beat1 0))
                (B2 (vector-ref beat2 0))
                (mcpb (midi-clocks-per-beat))
                (mbpb (midi-beats-per-bar))
                (tick (- t1 t2))
                (tick-n (modulo tick mcpb))
                (beat (- b1 b2))
                (beat-n (modulo (- beat (abs (quotient (- mcpb tick) mcpb))) mbpb))
                (bar-n (- B1 B2 (abs (quotient (- mbpb beat) mbpb))))
            )
            (vector bar-n beat-n tick-n)
        )
    )
    (define (beat-equal? beat1 beat2)
        (let
            (
                (result #t)
            )
            (for/first ((i (list 0 1 2)) #:when (not (equal? (vector-ref beat1 i) (vector-ref beat2 i))))
                (cond
                    ((> (vector-ref beat1 i) (vector-ref beat2 i))
                        (set! result 1)
                    )
                    ((< (vector-ref beat1 i) (vector-ref beat2 i))
                        (set! result -1)
                    )
                )
            )
            result
        )
    )
    (define (beat-translate beats)
        (let
            (
                (result (vector 0 0 0))
            )
            (for ((beat beats))
                (let
                    (
                        (tick (inexact->exact (* (midi-clocks-per-beat) (- beat (truncate beat)))))
                        (beat (inexact->exact (truncate beat)))
                    )
                    (set! result (beat-add result (vector 0 beat tick)))
                )
            )
            result
        )
    )
; hash
;  id
;   next beat
;   pointeur de beat
;   first-beat
;   last-beat
; hash
;  id-pattern
;   list de beat
;   list de sauvegarde

    (define (beat-reset id)
        (let*
            (
                (last-first-beat (third (hash-ref beat-memory id)))
                (beat-param (second (hash-ref beat-pattern-memory id)))
                (beat-seq (first (hash-ref beat-pattern-memory id)))
                (beat-sequence (beat-translate (list (fourth beat-param))))
                (first-beat (beat-add last-first-beat beat-sequence))
                (last-pattern-beat (beat-add first-beat beat-sequence))
                (last-beat-compute
                    (if (third beat-param)
                        last-pattern-beat
                        (letrec
                            (
                                (beat-compute-last
                                    (lambda (n v)
                                        (cond
                                            ((negative? (beat-equal? v last-pattern-beat))
                                                (cond
                                                    ((>= n (length beat-seq))
                                                        v
                                                    )
                                                    (else
                                                        (beat-add v (beat-translate (list (list-ref beat-seq n))))
                                                        (beat-compute-last (add1 n) v)
                                                    )
                                                )
                                            )
                                            (else
                                                v
                                            )
                                        )
                                    )
                                )
                            )
                            (beat-compute-last 0 first-beat)
                        )
                    )
                )
                (last-beat (beat-sub last-beat-compute (vector 0 0 1)))
                (beat-point 0)
                (next-beat (beat-add first-beat (beat-translate (list (list-ref (first (hash-ref beat-pattern-memory id)) beat-point)))))
            )
            (hash-set! beat-memory id (list next-beat beat-point first-beat last-beat))
        )
    )
    (define (beat-next-beat id midi-pos)
        (let*
            (
                (beat-point (second (hash-ref beat-memory id)))
                (beat-next-point (+ beat-point 1))
                (beat-list (first (hash-ref beat-pattern-memory id)))
                (beat (first (hash-ref beat-memory id)))
                (beat-next (beat-add beat (beat-translate (list (list-ref beat-list (min (sub1 (length beat-list)) beat-next-point))))))
            )
            (if (exact-nonnegative-integer? (beat-equal? beat-next (fourth (hash-ref beat-memory id))))
                (beat-reset id)
                (hash-set! beat-memory id (list beat-next beat-next-point (third (hash-ref beat-memory id)) (fourth (hash-ref beat-memory id))))
            )
        )
    )
    (define (beat-update id beat-param midi-pos)
        (let
            (
                (beat-pattern
                    (append (list (first beat-param)) (second beat-param))
                )
                    
            )
            (hash-set! beat-pattern-memory id (list beat-pattern beat-param))
            (hash-set! beat-memory id (list 0 0 (vector (vector-ref midi-pos 0) 0 0) 0))
            (beat-reset id)
        )
    )
    (define (beat-catch id name #:first-beat (first-beat 0) #:beat (beat-pattern (list 1)) #:mode-continuous (continuous-mode #t) #:beat-limit (beat-limit 4))
        (let ((id-name (string-append id name)) (mid-pos (midi-position)))
            (if (hash-has-key? beat-memory id-name)
                (unless (equal? (second (hash-ref beat-pattern-memory id-name)) (list first-beat beat-pattern continuous-mode beat-limit))
                    (beat-update id-name (list first-beat beat-pattern continuous-mode beat-limit) mid-pos)
                )
                (beat-update id-name (list first-beat beat-pattern continuous-mode beat-limit) mid-pos)
            )
;            (show "")
;            (show (first (hash-ref beat-memory id-name)))
;            (show mid-pos)
;            (show (beat-equal? mid-pos (first (hash-ref beat-memory id-name))))
            (cond
                ((exact-nonnegative-integer? (beat-equal? mid-pos (first (hash-ref beat-memory id-name))))
                    (beat-next-beat id-name mid-pos)
                    #t
                )
                (else
                    #f
                )
            )
        )
    )
)
