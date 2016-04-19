(define (cube-invasion-destroy id)
(show "cube-invasion-destroying")
    (travel-end (string-append (number->string id) "-" "cube-invasion"))
    (travel-end (string-append (number->string id) "-" "cube-invasion" "-r"))
    (rm-task (string->symbol (string-append (number->string id) "-" "cube-invasion")))
)

(define (cube-invasion id cross)
    (with-state
        (hint-ignore-depth)
        (let*
            (
                (ID (string-append (number->string id) "-" "cube-invasion"))
                (t  (travel ID (vector 0 0 -5) (vector 0 0 5) (c "speed" id #:coeff 5)))
                (tr (travel (string-append ID "-r") (vector 0 0 0) (vector 720 0 0) (c "speed" id #:coeff 5)))
            )
            (translate t)
            (translate (vector (c "trans" id) 0 0))
            (rotate tr)
            (colour (vector 0 1 0))
            (draw-cube)

            (when (>= (vector-ref t 2) 4.9)
                (cube-invasion-destroy id)
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
 
