(define (par-destroy ID)
    (travel-end ID)
    (travel-end (string-append ID "-2"))
    (travel-end (string-append ID "-3"))
    (rm-task (string->symbol ID))
)

(define (par id cross)
    (with-state
        (let*
            (
                (speed (c "par-speed" id))
                (ID (string-append (number->string id) "-" "par"))
                (t (travel ID (vector 1 0 0) (vector 0 0 0) speed))
                (t1 (travel (string-append ID "-2") (vector 0 0.5 0) (vector 0 0 0) (* speed 0.4)))
                (t2 (travel (string-append ID "-3") (vector 1 1 1) (vector 3 3 3) speed))
            )
            (translate (vector 0 0 (c "par-trans" id)))
            (rotate (vector 90 0 0))
            (scale (vector 5 1 5))
            (scale 0.2)
            (scale t2)
            (colour (vadd (vmul t1 (c "par-color" id)) t))
            (opacity (vector-ref t 0))
            (draw-cylinder)
            (when (<= (vector-ref t 0) 0.1)
                (par-destroy ID)
            )
        )
    )
)

;t0---------t0+speed
;    Y
;   t0+time
;t0
;dt

;v= d/t
;d=v/t
        
;v=dl/dt
 