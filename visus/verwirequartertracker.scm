(define (quartertracker id cross)
    (let*
        (
            (tuioControl (c "points" id #:type "Tuio"))
            (tuioCursors (PersistentTuioCursor tuioControl (* (c "persistTuioDelay" id) 10)))
        )
;; (show "quartertracker tuio")
;; (show tuioCursors)
        (unless (empty? tuioCursors)
        ;(show tuioCursors)
            (flxseed 1)
            (let*
                (
                    (gainC (* (c "gain" id) 30))
                    (firstBand (c "firstBand" id))
                    (secondBand (c "secondBand" id))
                    (ghOffset (c "gh-offset" id))
                    (sizeAll (* (c "size-all" id) 10))
                    (sizeCenter (* (c "center" id) 2))
                    (rotateList (map (lambda (r) (* 180 r)) (list (c "rotate-1" id) (c "rotate-2" id) (c "rotate-3" id))))
                    (rotate90 (* 90 (c "rotate-4" id)))
                    (tuioPoints (CalibrateTuioCursors tuioCursors (* 20 (c "tuioX" id)) (* 20 (c "tuioY" id)) (- (* 20 (c "tuioOffsetX" id)) 10) (- (* 20 (c "tuioOffsetY" id)) 10)))
                    (sortPoints (sort tuioPoints < #:key (lambda (p) (vector-ref p 0))))
                    (lengthPoints (length tuioPoints))
                    (halfPoints (/ lengthPoints 2))
                    (ceilhalfPoints (ceiling halfPoints))
                    (floorhalfPoints (floor halfPoints))
                    (symPoints (range (* floorhalfPoints -1) (+ floorhalfPoints 1) (max (/ (+ floorhalfPoints floorhalfPoints) (max 1 (- lengthPoints 1))) 1)))
                    (ghCoeff (/ (get-num-frequency-bins) (max (+ (floor halfPoints) 1))))
                    (ghPoints (map (lambda (p) (+ (* ghCoeff (abs p)) ghOffset)) symPoints))
                    ;(link-opa (c "link-opacity" id))
;;                     (link-opacity (c "link-opacity" id))
;;                     (link-gain (c "link-gain" id))
;;                     (tempGa (vector->list (vmul (ga) link-gain)))
;;                     (nGa (length tempGa))
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
                                (rotateBand (+ rotate90 (list-ref rotateList (inexact->exact (floor (* 3 (flxrnd)))))))
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
                #(when (> (length sortPoints) 1)
                    (for-each
                        (lambda (point nextPoint)
                            (let*
                                (
                                    (vec (vmul (vsub point nextPoint) -1))
                                    (vec+ (vadd vec point))
                                )
                                (for-each
                                    (lambda (g i)
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
