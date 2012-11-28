#lang scheme

(require "SchemeTuio.ss")
(provide (all-from-out "SchemeTuio.ss"))

(provide
    CalibrateTuioCursor
    CalibrateTuioCursors
    PersistentTuioCursor
)

(define (CalibrateTuioCursor cursor2D coeffX coeffY)
    (let
        (
            (coeffCursor
                (lambda (cursor cursorAxis coeff)
                    (* (- (* (cursorAxis cursor) 2) 1) coeff)
                )
            )
        )
        (vector (coeffCursor cursor2D 2Dcur-posX coeffX) (coeffCursor cursor2D 2Dcur-posY (* coeffY -1)) 0)
    )
)

(define (CalibrateTuioCursors cursors2D coeffX coeffY)
    (let
        (
            (coeffCursor
                (lambda (cursor cursorAxis coeff)
                    (* (- (* (cursorAxis cursor) 2) 1) coeff)
                )
            )
        )
        (map
            (lambda (cursor2D)
                (vector (coeffCursor cursor2D 2Dcur-posX coeffX) (coeffCursor cursor2D 2Dcur-posY (* coeffY -1)) 0)
            )
            cursors2D
        )
    )
)

(define persistent2Dcursors (make-hash))

(define (PersistentTuioCursor cursors2D delay)
    (let*
        (
            (secondDelay (* 1000 delay))
            (updateCursor '())
            (currentTime (current-inexact-milliseconds))
        )
        (for-each
            (lambda (cursor2D)
                (hash-set! persistent2Dcursors (2Dcur-sID cursor2D) (cons currentTime cursor2D))
            )
            cursors2D
        )
        (hash-for-each 
            persistent2Dcursors
            (lambda (id Time.persistCur)
                (cond
                    ((< (+ (car Time.persistCur) secondDelay) currentTime)
                        (hash-remove! persistent2Dcursors id)
                    )
                    (else
                        (set! updateCursor (append updateCursor (list (cdr Time.persistCur))))  
                    )
                )
            )
        )
        updateCursor
    )
)
        