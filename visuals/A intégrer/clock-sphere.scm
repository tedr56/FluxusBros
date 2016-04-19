(define (clock-sphere id cross)
     (let* (
             (pos (midi-position))
             (bar (vector-ref pos 0))
             (beat (vector-ref pos 1))
             (clock (vector-ref pos 2))
             ; duration of a bar in clocks
             (bar-in-clocks (* (midi-beats-per-bar) (midi-clocks-per-beat)))
             ; period of our animation = 2 bars
             ; compute how many clocks will be received during 2 bars
             (period-in-clocks (* 2 bar-in-clocks))
             ; compute how many clocks have been received since the beginning of the animation cycle
             (position-in-period (+ (* (modulo bar 2) 
(midi-beats-per-bar) (midi-clocks-per-beat)) (* beat 
(midi-clocks-per-beat)) clock))
             ; convert to a real between 0.0 and 1.0
             ; multiply by 1.0 to get a real result and not a fraction
             (animdelta (/ (* 1.0 position-in-period) (* 1.0 period-in-clocks)))
             )
         (draw-sphere)
         (with-state
             (rotate (vector 0 0 (* 360 animdelta)))
             (translate (vector 5 0 0))
             (colour (vector 1 1 0))
             (draw-sphere))))

;(every-frame (clock-sphere))

