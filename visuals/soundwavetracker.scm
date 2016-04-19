(define (soundwavetracker id cross)
    (let*
        (
            (tuioControl (c "points-wave" id #:type "Tuio"))
            (tuioCursors (PersistentTuioCursor tuioControl (* (c "persistTuioDelay" id) 10)))
        )
;; (show "soundwavetracker tuio")
;; (show tuioCursors)
        (unless (empty? tuioCursors)
            (let*
                (
;;                     (tuioPoints (CalibrateTuioCursors tuioCursors (* 10 (c "tuioX" id)) (* 10 (c "tuioY" id))))
                    (tuioPoints (CalibrateTuioCursors tuioCursors (* 20 (c "tuioX" id)) (* 20 (c "tuioY" id)) (- (* 20 (c "tuioOffsetX" id)) 10) (- (* 20 (c "tuioOffsetY" id)) 10)))
                    (sortPoints (sort tuioPoints < #:key (lambda (p) (vector-ref p 0))))
                    ;(link-opa (c "link-opacity" id))
                    (link-opacity (c "link-opacity" id))
                    (link-gain (* (c "link-gain" id) 0.05))
                    (tempGa (vector->list (vmul (ga) link-gain)))
                    (nGa (length tempGa))
                )
                (when (> (length sortPoints) 1)
                    (for-each
                        (lambda (point nextPoint)
                            (let*
                                (
                                    (vec (vmul (vsub point nextPoint) -1))
                                    (vec+ (vadd vec point))
                                )
                                (for-each
                                    (lambda (g i)
                                        (when (even? i)
                                            (let*
                                                (
                                                    (va (vector 0 0 g))
                                                    (vl
                                                        (if (zero? i) 1 (/ nGa i))
                                                    )
                                                    (vs (vadd point (vdiv vec vl)))
                                                    (vd (vadd vs (vmul (vcross vec va) 1)))
                                                )
                                                (with-state
                                                    (wire-colour (vector 0 1 0))
                                                    (wire-opacity link-opacity)
                                                    (draw-line vs vd)
                                                )
                                            )
                                        )
                                    )
                                    tempGa
                                    (build-list nGa values)
                                )
                            )
                        )
                        sortPoints
                        (append (drop sortPoints 1) (list (list-ref sortPoints 0)))
                    )
                )
            )
        )
    )
    #t
)
