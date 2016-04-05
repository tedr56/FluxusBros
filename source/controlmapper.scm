
;Trigger Interface
;setVisu
;setFocus
;(re)loadTriggerControls
;(re)loadVisualControls

(define ControlMapper%
    (class object%
        (init-field
            Players
            Mappings
        )
        (field
            (Map (make-hash))            ;Hash#[TableControl%] '(player FilterControl%)
            (MapTuio (make-hash))
            (numTypesEvents (make-hash)) ;Hash#[Type]numControl
            (TypesEvents (make-hash))    ;Hash#[Type]funct - for controlThread Queue
            (EventThread (launchThread))

            (KbDown '())                ;'(Keys-Down) buffer save for keys up parsing
            (KbsDown '())                ;'(Keys-Special-Down) buffer save for special keys up parsing
            (TuioDown #f)                ; Retain if last tuio message contained cursors to send one empty message
            (last-osc-peek "no message yet...")
        )
        (define/public (getTypesEvents)
      (show TypesEvents)
    )
    (define/public (get-players)
      Players
    )
        (define/public (recordControl table-filterControls) ; table-filterControls '(TableControl% FilterControl%))
            (show-d "->recordControl ")
            (for-each
                (lambda (table-filterControl)
                    (cond
                        ((equal? "tuio" (send (first table-filterControl) getType))
                            (hash-set! MapTuio (second table-filterControl) #t)
                            (setTypeCounts (send (first table-filterControl) getType) 1)
                        )
                        ((not (hash-has-key? Map (first table-filterControl)))  ; add a new control if no previous control recorded ; TODO : multi '(VisualControl% TriggerControls%) destination
                            (hash-set! Map (first table-filterControl) (second table-filterControl))
                            (setTypeCounts (send (first table-filterControl) getType) 1)
                        )
                    )
                )
                table-filterControls
            )
            (show-d "  recordControl->")
        )
        (define/public (unRecordControl tableControls #:force (force #f)) ; tableControls '(TableControl%) ; TODO : send VisualControl setPlayer #f
            (for-each
                (lambda (tableControl)
                    (cond
                        ((equal? "tuio" (send (first tableControl) getType))
                            (when force
                                (hash-remove! MapTuio (second tableControl))
                                (setTypeCounts (send (first tableControl) getType) -1)
                            )
                        )
                        (else
                            (when (hash-has-key? Map (first tableControl))
                                (send (hash-ref Map (first tableControl)) setPlayer #f)
                                (hash-remove! Map (first tableControl))
                                (setTypeCounts (send (first tableControl) getType) -1)
                            )
                        )
                    )
                )
                tableControls
            )
        )
        (define/private (setTypeCounts type add)
            (hash-set! numTypesEvents type (max (+ (hash-ref numTypesEvents type 0) add) 0))
            (if (zero? (hash-ref numTypesEvents type))
                (hash-remove! TypesEvents type)
                (unless (hash-has-key? TypesEvents type)
                    (unless (equal? type "fake")
                        (hash-set! TypesEvents type (getEventFunct type))
                    )
                )
            )
        )
        (define/private (getEventFunct type)
        (show type)
            (cond
                ((equal? type "midi-ccn")
                    (lambda () (ccEvents))
                )
                ((equal? type "midi-note")
                    (lambda () (noteEvents))
                )
                ((equal? type "kb")
                    (lambda () (kbEvents))
                )
                ((equal? type "kbs")
                    (lambda () (kbsEvents))
                )
                ((equal? type "osc")
                    (osc-source-launch)
                    (lambda () (oscEvents))
                )
                ((equal? type "tuio")
                    (tuio-source-launch)
                    (lambda () (tuioEvents))
                )
            )
        )
        (define/public (getMap)
            Map
        )
        (define/public (controlThread) ;main TableControl thread mapping
            (hash-for-each
                TypesEvents
                (lambda (type funct)
                    (funct)
                )
            )
            #t
        )
        (define/private (launchThread)
            (spawn-task (lambda () (controlThread)) 'ControlTask)
        )
        (define/private (kbEvents)
            (let*
                (
                    (newDown (keys-down))
                    (keysUp (remove* newDown KbDown))
                    (keysDown (remove* KbDown newDown))
                )
                ; TODO : Send KbDown & KbUp Messages to VisualControl% or TriggerControl%
                (set! KbDown newDown)
                (for-each
                    (lambda (k)
                        (updateVisualControl (new TableControl% (type "kb") (address k)) (list 0))
                    )
                    keysUp
                )
                (for-each
                    (lambda (k)
                        (updateVisualControl (new TableControl% (type "kb") (address k)) (list 127))
                    )
                    keysDown
                )
            )
        )
        (define/private (kbsEvents)
            (let*
                (
                    (newDown (keys-down))
                    (keysUp (remove* newDown KbsDown))
                    (keysDown (remove* KbDown newDown))
                )
                ; TODO : Send KbDown & KbUp Messages to VisualControl% or TriggerControl%
                (set! KbDown newDown)
                (for-each
                    (lambda (k)
                        (updateVisualControl (new TableControl% (type "kb") (address k)) (list 0))
                    )
                    keysUp
                )
                (for-each
                    (lambda (k)
                        (updateVisualControl (new TableControl% (type "kb") (address k)) (list 127))
                    )
                    keysDown
                )
            )
            1
        )
        (define/private (ccEvents (e (midi-cc-event)))
                (when e
                    (let ((eNext (midi-cc-event)))
                    (cond
                        (eNext
                            (cond
                                ((equal? (vector-take e 2) (vector-take eNext 2))
                                    (ccEvents eNext)
                                )
                                (else
                                    (let
                                        (
                                            (Table (new TableControl% (type "midi-ccn") (address  (vector-take e 2))))
                                            (Value (vector-ref e 2))
                                        )
                                        (updateVisualControl Table (list Value))
                                    )
                                    (ccEvents eNext)
                                )
                            )
                        )
                        (else
                            (let
                                (
                                    (Table (new TableControl% (type "midi-ccn") (address  (vector-take e 2))))
                                    (Value (vector-ref e 2))
                                )
                                (updateVisualControl Table (list Value))
                            )
                        )
                    )
                )
            )
        )
        (define/private (noteEvents)
            (let ((e (midi-note)))
                (when e
                    (let
                        (
                            (Table (new TableControl% (type "midi-note") (address (vector (vector-ref e 1) (vector-ref e 2)))))
                            (Value
                                (if (equal? (vector-ref e 0) 'note-off)
                                    0
                                    (vector-ref e 3)
                                )
                            )
                        )
                        (updateVisualControl Table (list Value))
                    )
                    (noteEvents)
                )
            )
        )
        (define/private (oscEvents)
            (let ((peek (osc-peek)))
                (unless (equal? peek last-osc-peek)
                    (let
                        (
                            (split-peek (string-split peek))
                        )
                        (when (>= (length split-peek) 3)
                            (let*
                                (
                                    (path-peek (first split-peek))
                                    (type-peek (second split-peek))
                                    (msg-convert '())
                                    (msg-peek (drop split-peek 2))
                                    (newOscControl (new TableControl% (type "osc") (address path-peek)))
                                    (convert-msg
                                        (lambda ()
                                            (for-each
                                                (lambda (i)
                                                    (let ((type (string-ref type-peek i)))
                                                        (cond
                                                            ((equal? type #\i)
                                                                (set! msg-convert (append msg-convert (list (string->number (list-ref msg-peek i)))))
                                                            )
                                                            ((equal? type #\f)
                                                                (set! msg-convert (append msg-convert (list (string->number (list-ref msg-peek i)))))
                                                            )
                                                            ((equal? type #\s)
                                                                (set! msg-convert (append msg-convert (list (list-ref msg-peek i))))
                                                            )
                                                            (else (show-d "Osc Error Convert"))
                                                        )
                                                    )
                                                )
                                                (build-list (length msg-peek) values)
                                            )
                                        )
                                    )
                                )
                                (convert-msg)
                                (cond
                                    ((equal? "FluxusBros" (first (string-split path-peek "/")))
                                        (updateInterface newOscControl msg-convert)
                                    )
                                    (else
                                        (updateVisualControl newOscControl msg-convert)
                                    )
                                )
                            )
                            (set! last-osc-peek peek)
                            (oscEvents)
                        )
                    )
                )
            )
        )
        (define/private (tuioEvents)
            (let ((peekCursor (get-tuio-cursors)))
                (cond
                    ((empty? peekCursor)
                        (cond
                            (TuioDown
                                (let ((newTuioControl (new TableControl% (type "tuio") (address #t))))
                                    (set! TuioDown #f)
                                    (updateVisualControlTuio newTuioControl peekCursor)
                                )
                            )
                        )
                    )
                    (else
                        (let ((newTuioControl (new TableControl% (type "tuio") (address #t))))
                            (set! TuioDown #t)
                            (updateVisualControlTuio newTuioControl peekCursor)
                        )
                    )
                )
            )
        )
        (define/private (updateVisualControlTuio tableC Val (table (hash->list Map)) (notfound-funct (lambda (C) (void))))
            (hash-for-each
                MapTuio
                (lambda (key val)
                    (send key updateControl Val)
                )
            )
        )
        (define/private (updateVisualControl tableC Val (table Map) (notfound-funct (lambda (C) (void))))
            (let ((match (hash-ref table tableC #f)))
                (cond
                    (match
                        (send match updateControl Val)
                        #t
                    )
                    (else
                        (notfound-funct tableC)
                        #f
                    )
                )
            )
        )
        (define/private (updateInterface oscObj msg)
            (let*
                (
                    (splitPath (string-split (send oscObj getAddress) "/"))
                    (typeOsc (second splitPath))
                )
                (cond
                    ((equal? typeOsc "mapping")
                        (when (and (= (length splitPath) 4) (= (length msg) 1))
;;                     (Mapping% (setStripPoint strip point pos))
                            (send Mappings setStripPoint (third splitPath) (fourth splitPath) (first msg))
                        )
                    )
                    ((hash-has-key? typeOsc Players)
                        1
                    )
                    ((number? typeOsc)
                        1
                    )
                )
            )
        )
        (super-new)
    )
)

