(clear)

; pixels primitive with 2 textures and active renderer
(define p (build-pixels 1024 1024 #t 2))
(with-primitive p ; hide it
    (scale 0))

; extra camera
(define cam1 (with-pixels-renderer p
        (build-camera)))

(with-pixels-renderer p
    (current-camera cam1)
    (viewport 0.5 0 0.5 1) ; use the right handside for the extra camera
    (set-camera (mmul (mtranslate (vector 0 0 -2))
                (mrotate (vector 0 -145 0))))

    (current-camera 0) ; left handside for the default camera
    (viewport 0 0 0.5 1))

; fullscreen quad for displaying the scene
(define fs-quad (with-state
                    (hint-unlit)
                    (scale #(20 15 1))
                    (build-plane)))

; set its texture coordinates to the default camera viewport
; in the pixel primitive
(with-primitive fs-quad
    (pdata-set! "t" 0 #(0 1 0))
    (pdata-set! "t" 1 #(.5 1 0))
    (pdata-set! "t" 2 #(.5 0 0))
    (pdata-set! "t" 3 #(0 0 0)))

; scene object
(define c (with-pixels-renderer p
             (build-cube)))

(with-pixels-renderer p
    (with-primitive c
        (hint-vertcols)
        (pdata-map! (lambda (c) (rndvec)) "c")))

; camera display
(define tv (with-pixels-renderer p
                (hint-unlit)
                (build-plane)))

(with-pixels-renderer p
    (with-primitive tv
        (hint-wire)
        (hide 1)
        (camera-hide 0)    ; show only in the default camera
        (rotate #(0 35 0))
        (translate #(0 0 -5))
        (scale #(6 8 1))

        ; set texture coords to the extra camera viewport
        (pdata-set! "t" 0 #(.5 1 0))
        (pdata-set! "t" 1 #(1 1 0))
        (pdata-set! "t" 2 #(1 0 0))
        (pdata-set! "t" 3 #(.5 0 0))))

; frame count to ping-pong render target
(define frame-count 0)


(every-frame
  (let ([tex (pixels->texture p (remainder frame-count 2))]
        [ptex (pixels->texture p (remainder (+ frame-count 1) 2))])

    ; set render targets and display texture for the pixel primitive
    (with-primitive p
        (pixels-render-to tex)
        (pixels-display tex))

    ; fullscreen quad texture
    (with-primitive fs-quad
        (texture tex))

    (with-pixels-renderer p
           ; rotate the cube
           (with-primitive c
                (identity)
                (rotate (vector (* 40 (time)) 35 0)))
           ; camera display texture
           (with-primitive tv
                (texture ptex)))

    (set! frame-count (+ frame-count 1))))
