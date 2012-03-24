(clear)

(scale (vector 20 15 1))

(define p (build-pixels 512 512 #t 1))

(define frame 0)

(every-frame
    (let ([tex (pixels->texture p frame)])
        (with-primitive p
            (pixels-render-to tex)
            (pixels-display tex))
        (with-pixels-renderer p
            (clear-colour 1)
            (with-state
                (texture tex)
                (rotate (vector (* 60 (time)) 45 0))
                (scale 6)
                (draw-cube)))))

