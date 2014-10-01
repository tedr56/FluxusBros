(define rot 0) ; a global variable is used to store the current rotation for all cogs

; a procedure to draw one cog
(define (draw-cog texfilename id)
    (define (ghp harmonic)
        (if (<= harmonic 1)
            (gh 2)
        (if (>= harmonic 16)
            (gh 15)
            (gh harmonic))))
    (push)                                   ; push a new state to work in
    (texture (load-texture texfilename))     ; set the current texture 
    (hint-unlit)                             ; turn opengl lighting off
    (hint-ignore-depth)                      ; needs to be on for transparency (ask)
    (translate (vmul (vector (flxrnd) (flxrnd) 0) 3)) ; move to a random position

    (rotate (vector 0 0 (* rot (* 10 (- (flxrnd) 0.5))))) ; rotate and make the speed 
                                             ; different for each cog
    (rotate (vmul (vector 0 0 (gh (* (flxrnd) 16))) (c "rot" id)))

    (let ((inde (* (flxrnd) 16)))
        (translate (vmul (vector 0 0 (gh inde)) (c "trans" id #:coeff 3)))
        (scale (vadd (vmul (vector (ghp inde) (ghp inde) (ghp inde)) (* (c "scale1" id) (+ 1 (c "scale2" id)))) (vmul (vector 1 1 1) (c "scale-coeff" id))))
)
    (draw-plane)                             ; draw the plane
    (pop))                                   ; pop this state
   

(define (draw-all-cogs id)
    (draw-cog "flower/activate.png" id) ; draw cogs for all the textures...
    (draw-cog "flower/daisy.png" id)
    (draw-cog "flower/flower.png" id)
    (draw-cog "flower/flower2.png" id)
    (draw-cog "flower/flower3.png" id)
    (draw-cog "flower/flower-old.png" id)
    (draw-cog "flower/pause.png" id)
    (draw-cog "flower/tokenbg.png" id)
    (draw-cog "flower/tokenbg.png" id)
)
    
(define (flower id cross)
    (gain (* (c "gain1" id) (c "gain2" id #:coeff 3)))
    (blur (mn 0 10))
    (flxseed 1) ; seed the random number generator, so we get the same sequence
                ; of random numbers every frame

    (push)                        ; make a new state for all the cogs
    (wire-colour (vector 1 1 1))
;    (hint-wire)                  ; uncomment to see the polygons
    (scale (vector 5 5 5))        ; make it bigger so we can zoom in
    (draw-all-cogs id)               ; draw all the cogs a few times
    (draw-all-cogs id)
    (draw-all-cogs id)
    (draw-all-cogs id)
    (pop)
    (set! rot (+ rot 1)))         ; increment the global rotation counter

;(every-frame (render))
