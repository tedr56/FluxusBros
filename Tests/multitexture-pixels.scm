(clear)
(clear-colour 0.1)
(reset-camera)
(define s 5)
(define mask (with-state (scale s) (translate (vector 1 0 0)) (build-pixels 500 500 #t)))

(define renderer
    (with-state
        (scale s)
        (translate (vector -1 0 0))
        (build-pixels 500 500 #t)
    )
)
(define mapping
    (with-state
        (translate (vector 0 0 .1))
        (scale s)
        (multitexture 0 (pixels->texture renderer))
        (texture-params 0
            (list
                'min 'nearest
                'mag 'nearest
                'wrap-t 'clamp
                'wrap-s 'clamp
                'tex-env 'replace
            )
        )
        (multitexture 1 (pixels->texture mask))
        (texture-params 1
            (list
                'min 'nearest
                'mag 'nearest
                'wrap-s 'clamp
                'wrap-t 'clamp
                'tex-env 'blend
            )
        )
        (build-plane)
    )
)
#(with-primitive p
    (show (pdata-exists? "t"))
    (pdata-map!
        (lambda (t)
            (vmul t 1)
        )
        "t"
    )
)
#(with-pixels-renderer mask
    (build-cube)
)
(define (map-mask)
    (with-pixels-renderer mask
        (with-state
            (clear-colour #(1 1 1))
            (colour (vector 0 0 0))
            (rotate (vmul (vector 0 (time) (time)) 20))
            (scale 5)
            (draw-cube)
        )
    )
    (with-pixels-renderer renderer
        (with-state
            (clear-colour #(0.2 0.2 0.2))
            (rotate (vmul (vector (time) (time) 0) 20))
            (scale 5)
            (draw-cube)
        )
    )
)

(every-frame (map-mask))
