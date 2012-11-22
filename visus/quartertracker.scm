(define (quartertracker id cross)
    (let*
        (
            (tuioPoints (c "points" id #:type "Tuio"))
        )
        (for-each
            (lambda (point)
;;                 (show (vector (2Dcur-vecX point) (2Dcur-vecY point)))
                (show (2Dcur-path point))
                (with-state
                    (translate (vector (2Dcur-posX point) (2Dcur-posY point) 0))
                    (draw-cube)
                )
            )
            tuioPoints
        )
    )
    #t
)