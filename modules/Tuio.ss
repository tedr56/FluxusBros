#lang scheme

(require "SchemeTuio.ss")
(provide (all-from-out "SchemeTuio.ss"))

(provide
    CalibrateTuioCursor
    CalibrateTuioCursors 
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
