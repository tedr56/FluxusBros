(define BlResizeList (list blbl0 blbl1 blbl2 blbl3))
(define BlResizePrims (make-hash))
(define (BlResize-build id)
    (hash-set!
        BlResizePrims
        (map
            (lambda (prim)
                (define copy (build-copy prim))
                (with-primitive copy
                    (hide 0)
                    (pdata-add "pos" "v")
                    (pdata-copy "p" "pos")
                )
                copy
            )
            BlResizeList
        )
    )
)
(define (BlResize-destroy id)
    (for-each
        (lamnda (prim)
            (destroy prim)
        )
        (hash-ref BlResizePrims id)
    )
    (hash-remove! BlResizePrims id)
)

(define (BlResize id cross)
    (for-each
        (lambda (i)
            (with-primitive (list-ref (hash-ref BlResizePrims id) i)
                (pdata-index-map!    
                    (lambda (i p)
                        (vector
            )
        )
        (build-list 4 values)
    )
) 