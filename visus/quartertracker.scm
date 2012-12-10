(define (quartertracker id cross)
    (let
        (
            (tuioCursors (PersistentTuioCursor (c "points" id #:type "Tuio") (* (c "persistTuioDelay" id) 10)))
        )

        (unless (empty? tuioCursors)
            (flxseed 1)
            (let*
                (
                    (gainC (* (c "gain" id) 3))
                    (firstBand (c "firstBand" id))
                    (secondBand (c "secondBand" id))
                    (ghOffset (c "gh-offset" id))
                    (sizeAll (* (c "size-all" id) 10))
                    (sizeCenter (* (c "center" id) 2))
                    (rotateList (map (lambda (r) (* 180 r)) (list (c "rotate-1" id) (c "rotate-2" id) (c "rotate-3" id) (c "rotate-4" id))))
                    (tuioPoints (CalibrateTuioCursors tuioCursors (* 10 (c "tuioX" id)) (* 10 (c "tuioY" id))))
                    (sortPoints (sort tuioPoints < #:key (lambda (p) (vector-ref p 0))))
                    (lengthPoints (length tuioPoints))
                    (halfPoints (/ lengthPoints 2))
                    (ceilhalfPoints (ceiling halfPoints))
                    (floorhalfPoints (floor halfPoints))
                    (symPoints (range (* floorhalfPoints -1) (+ floorhalfPoints 1) (max (/ (+ floorhalfPoints floorhalfPoints) (max 1 (- lengthPoints 1))) 1)))
                    (ghCoeff (/ (get-num-frequency-bins) (max (+ (floor halfPoints) 1))))
                    (ghPoints (map (lambda (p) (+ (* ghCoeff (abs p)) ghOffset)) symPoints))
                )
                (for-each
                    (lambda (point ghPoint)
                        (with-state
                            (hint-normalise)
                            (translate point)
                            (translate (vector 0 0 0.5))
                            (scale (vector sizeAll sizeAll sizeCenter))
                            (draw-cube)
                        )
                        (let*
                            (
                                (rotateBand (list-ref rotateList (inexact->exact (floor (* 4 (flxrnd))))))
                                (size-y (gl ghPoint gainC))
                                (size-y-first (* firstBand size-y))
                                (size-y-second (* secondBand size-y))
                            )
                            (with-state
                                (hint-normalise)
                                (translate point)
                                (rotate (vector 0 0 (+ rotateBand 180)))
                                (translate (vector 0 (/ size-y-first 2) 0))
                                (scale (vector sizeAll size-y-first sizeAll))
                                (draw-cube)

                            )
                            (with-state
                                (hint-normalise)
                                (translate point)
                                (rotate (vector 0 0 rotateBand))
                                (translate (vector 0 (/ size-y-second 2) 0))
                                (scale (vector sizeAll size-y-second sizeAll))
                                (draw-cube)
                            )
                        )
                    )
                    sortPoints
                    ghPoints
                )
            )
        )
    )
    #t
)