;; example of using an ffgl plugin on a pixel primitive

(clear)

(scale #(20 15 1))

; pixel primitive with 2 textures and an active renderer
(define p (build-pixels 512 512 #t 2))

(define plugin (ffgl-load "cibloom" 512 512))

(with-ffgl plugin
    (ffgl-process p ; output pixel primitive
                  (pixels->texture p 1) ; output texture of the pixel primitive
                  (pixels->texture p 0) ; input texture
                  ))

(with-primitive p
    ; the renderer of the pixels primitive renders to texture index 0
    (pixels-render-to (pixels->texture p 0))
    ; the primitive is displayed using texture index 1, the output texture
    ; of the ffgl plugin
    (pixels-display (pixels->texture p 1)))

(define (anim)
    (with-pixels-renderer p
        (with-state
            (clear-colour 0)
            (scale 5)
            (rotate (vector (* 50 (time)) -17 (* -35 (time))))
            (draw-cube))))

(every-frame (anim))
