(define (traveller-destroy id)
(show "traveller-destroy")
    (travel-end (string-append (number->string id) "-" "traveller"))
    (travel-end (string-append (number->string id) "-" "traveller" "-r"))
    (rm-task (string->symbol (string-append (number->string id) "-" "traveller")))
)

(define (traveller id cross)
    (with-state
        (let*
            (
                (ID (string-append (number->string id) "-" "traveller"))
        (speed (c "speed" id))
                (t  (travel ID (vector -5 0 0) (vector 5 0 0) speed))
                (tr (travel (string-append ID "-r") (vector 0 0 0) (vector 0 0 -720) speed))
            )
            (translate t)
            (rotate tr)
            (colour (vector 0 1 0))
            (draw-cube)

            (when (>= (vector-ref t 0) 4.9)
                (traveller-destroy id)
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
 
