
(define Visual%
    (class object%
        (init-field
            name
            crosslevel
            (visualControlList (make-hash))
        )
        (field
            (file (string-append DEFAULT_VISUAL_PATH name ".scm"))
            (running #f)
            (task-name (string->symbol (string-append crosslevel "-" name)))
        )
        (define/public (running?)
            running
        )
        (define/public (setVisualControls controlList)  ; controlList '('(TableControl% VisualControl%))
            (show-d "->setVisualControls ")
            (for-each
                (lambda (visualControlParams)
                    (hash-set! visualControlList (send (third visualControlParams) getName) (third visualControlParams))
                )
                controlList
            )
            (show-d "  setVisualControls->")
        )
        (define/public (Start)
            (unless running
                (
                    (eval-string
                        (string-append
                            name
                            "-"
                            "build"
                        )
                        (lambda (exn)
                            (show "fonction build absente")
                            void
                        )
                    )
                    this
                )
                (show-d "Lancement de visuel")
                (show-d name)
                ;(thread (with-state ((eval-string name) this 1)))
                (spawn-task (lambda () (with-state ((eval-string name) this 1))) (get-visu-task-name))
                (set! running #t)
            )
        )
        (define/public (Stop)
            (when running
                (
                    (eval-string
                        (string-append
                            name
                            "-"
                            "destroy"
                        )
                        (lambda (e)
                            void
                        )
                    )
                    this
                )
                (when (task-running? (get-visu-task-name))
                    (rm-task (get-visu-task-name))
                )
                (set! running #f)
            )
        )
        (define/public (getControl N #:Type (T "number"))
            ;(send (hash-ref! visualControlList N (new VisualControl% (name N) (Value 0.5) (Player #f))) getControl)
            (send (hash-ref visualControlList N) getControl)
        )
        (define/public (getControls)
            visualControlList
        )
        (define/public (getCrossLevel)
            crosslevel
        )
        (define/private (get-visu-task-name)
            task-name
        )
        (define/public (get-name)
            task-name
        )
        (define/public (getName)
            name
        )
        (define/public (getVisual)
            name
        )
        (define/public (getId)
            (string->number crosslevel)
        )
        (super-new)
    )
)
