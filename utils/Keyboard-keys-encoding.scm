(define (show-keys)
    (let
        (
            (kb (keys-down))
            (kbs (keys-special-down))
            )
        (when (> (length kb) 0)
            (print "keys pressed")
            (newline)
            (print kb)
            (newline)
            )
        (when (> (length kbs) 0)
            
            (print "keys special pressed")
            (newline)
            (print kbs)
            (newline)
            )
        )
    )
(every-frame (show-keys))