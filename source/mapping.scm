
(define Strip%
    (class object%
        (field
            (ProjPrim #f)
            (MapPrim #f)
            (ProjPoints '())
            (MapPoints '())
        )
        (define/public (setProjPoint point pos)
            (cond
                (pos
                    (set! ProjPoints (list-insert ProjPoints pos point))
                    (set! MapPoints (list-insert MapPoints pos (midpoint MapPoints pos)))
                    (updatePrims)
                )
                ((equal? point #f)
                    (set! Projoints (list-remove ProjPoints pos))
                    (set! Mapjoints (list-remove ProjPoints pos))
                    (updatePrims)
                )
            )
        )
        (define/public (setMapPoint point pos)
            (cond
                (pos
                    (set! MapPoints (list-insert ProjPoints pos point))
                    (set! ProjPoints (list-insert MapPoints pos (midpoint MapPoints pos)))
                    (updatePrims)
                )
                ((equal? point #f)
                    (set! MapPoints (list-remove ProjPoints pos))
                    (set! Projoints (list-remove ProjPoints pos))
                    (updatePrims)
                )
            )
        )
        (define/private (updatePrims)
            (when ProjPrim
                (let ((ProjPrimPoints (with-primitive ProjPrim (pdata-size))))
                    (unless (= ProjPrimPoints ProjPoints)
                        (destroy ProjPrim)
                    )
                )
            )
            (when MapPrim
                (let ((MapPrimPoints (with-primitive MapPrim (pdata-size))))
                    (unless (= MapPrimPoints MapPoints)
                        (destroy MapPrim)
                    )
                )
            )
            (set! ProjPrim (build-polygon (length ProjPoints) 'polygon))
            (set! MapPrim (build-polygon (length ProjPoints 'polygon)))
            (when (= (length ProjPoints) (length MapPoints))
                (with-primitive ProjPrim
                    (pdata-index-map!
                        (lambda (p)
                            (list-ref ProjPoints i)
                        )
                        "p"
                    )
                )
                (with-primitive MapPrim
                    (pdata-index-map!
                        (lambda (p)
                            (list-ref MapPoints i)
                        )
                        "p"
                    )
                )
            )
        )
        (super-new)
    )
)

(define Mapping%
    (class object%
        (field
            (StripList '())
            (StripConfig (make-hash))
        )
        (define/public (setStripPoint strip point pos)
            (let ((targetStrip (getStrip strip)))
                (when targetStrip
                    (send targetStrip setProjPoint point pos)
                )
            )
        )
        (define/public (setMapPoint strip point pos)
            (let ((targetStrip (getStrip strip)))
                (when targetStrip
                    (send targetStrip setMapPoint point pos)
                )
            )
        )
        (define/private (getStrip strip)
            (cond
                ((hash-has-key? StripList strip)
                    (list-ref StripList strip)
                )
                (else
                    (cond
                        ((= strip (length StripList))
                            (set! StripList (append StripList (new Strip%)))
                            (list-ref StripList strip)
                        )
                        (else #f)
                    )
                )
            )
        )
        (define/public (setVisualMapConfig crosslevel config)
            1
        )
        (define/public (getRenderer crosslevel)
            1
        )
        (define/private (getConfig crosslevel)
            (cond
                ((hash-has-key? StripConfig crossLevel)
                    (hash-key StripConfig crossLevel)
                )
                (else
                    (cond
                        ((empty? StripList)
                            (list 0)
                        )
                        (else
                            1
                        )
                    )
                )
            )
        )
        (super-new)
    )
)

