
(define AbstractControl%
    (class object%
        (super-new)
        (init-field
            Value
            Player
            Type
            (id (gensym "AbsCtrl"))
            ;Callback     TODO OSC Callback
        )
        (field
            (valueLength (initValueLength))
            (filterFunction (initFilterPlayerFunction))
        )
        (define/private (initValueLength)
            (cond
                ((or (equal? Type "number") (equal? Type "coord1D") (equal? Type "string"))
                    1)
                ((or (equal? Type "coord2D")) 2)
                ((or (equal? Type "coord3D") (equal? Type "colour")) 3)
                (else 1)
            )
        )
        (define/private (initFilterPlayerFunction)
            (cond
                ((equal? Type "number")
                    (lambda (val) (and (> val (- (getValue) DEFAULT_HOOK_INTERVAL)) (< val (+ (getValue) DEFAULT_HOOK_INTERVAL))))
                )
                ((equal? Type "Tuio")
                    (lambda (val) #t)
                )
                (else
                    (lambda (val) (equal? val Value))
                )
            )
        )
        (define/public (updateControl val player)
            (filterUpdateValue val player)
        )
        (define/pubment (filterUpdateValue val player)
            (cond
                ((equal? Player player)
                    (setValue val)
                )
                (else
                    (when (filterFunction val)
                        (set! Player player)
                    )
                )
            )
            Value
        )
        (define/public (setPlayer player)
            (set! Player player)
        )
        (define/pubment (setValue val)
            (set! Value val)
            (inner (void) setValue val)
        )
        (define/public (getType)
            Type
        )
        (define/public (getValueLength)
            valueLength
        )
        (define/pubment (getControl)
            (inner (void) getControl)
            Value
        )
        (define/public (getValue)
            (getControl)
        )
    )
)

(define VisualControl%
    (class AbstractControl%
        (init-field
            name 
            (defaultValues '())
            (level #f)
        )
        (field
            (initLevel (initLevelDefault))
        )
        (inherit-field Player Type Value id)
        ;(inherit getValue setValue)
        (inherit getValue)
        (define/private (initLevelDefault)
            (show-d "->initLevelDefault")
            (show-d level)
            (when level
                (setLevel level)
            )
        )
        (define/public (setLevel lev)
            (show-d "->setLevel")
            (set! level (min lev (- (length defaultValues) 1)))
            (show-d (getValue))
            (setValue (list-ref defaultValues level))  ; TODO Task for fading VisualControl to next level
            (show-d (getValue))
        )
        (define/public (setDefaultValues valuesList)
            (set! defaultValues valuesList)
        )
        (define/public (getDefaultValues)
            defaultValues
        )
        (define/public (getName)
            name
        )
        (define/augment (setValue val)
            1
            #(when (equal? Type "Tuio")
                (show "->setControl")
                (show name)
                (show Value)
                (show Player)
                (show Type)
                (show id)
            )
        )
        #(define/augment (getControl)
            (when (equal? Type "Tuio")
                (show "->getControl")
                (show name)
                (show Value)
                (show Player)
                (show Type)
                (show id)
            )
        )
        (super-new)
    )
)

(define TriggerControl%
    (class AbstractControl%
        (super-new)
        (init-field
            (onTrigg #f)
            (offTrigg #f)
            (onRule (lambda (valT) (if (> valT DEFAULT_THRESHOLD_TRIGGER_VALUE) #t #f)))
            (offRule (lambda (valT) (if (<= valT DEFAULT_THRESHOLD_TRIGGER_VALUE) #t #f)))
        )
        (field
            (triggerStatus #f) ;
        )
        (define/augment (setValue val)
            (cond
                ((and (not triggerStatus) (onRule val))
                    (when onTrigg
                        (display "onTrigg : ")(show onTrigg)
                        (eval-string onTrigg (lambda (error) (show-d error)))
                    )
                    (set! triggerStatus #t)
                )
                ((and triggerStatus (offRule val))
                    (when offTrigg
                        (display "offTrigg : ")(show offTrigg)
                        (eval-string offTrigg (lambda (error) (show-d error)))
                    )
                    (set! triggerStatus #f)
                )
            )
        )
    )
)


; TODO MappingControl%
(define FilterControl%
    (class object%
        (init-field
            Abstract             ;reference to AbstractControl% for getValue()
            tablePlayer         ;player who own the TableControl%
            (eventpart 0)       ;value(s) to take from Table event
            (valcoeff 127)      ;maximum value of Table event, for coeff proportion
            (assign "assign")   ;type of assignement, "assign" "increment" "decrement"
            (abstractpart 0)    ;value(s) of AbstractControl% to send/modify
        )
        (field
            (abstractLength (send Abstract getValueLength))
            (abstractType (send Abstract getType))
            (AbstractTableParts #f)                             ;set by (initFilterFunction)
            (CoeffFunction #f)                                  ;set by (initFilterFunction)
            (FilterFunction (initFilterFunction))
        )
        (define/private (initFilterFunction)
            (show-d "->initFilterFunction")
            (show-d abstractType)
            (cond
                ((or (equal? abstractType "number") (equal? abstractType "coord1D"))
                    (set! valcoeff (checkNumListType valcoeff "number" DEFAULT_VAL_COEFF))
                    (set! eventpart (checkNumListType eventpart "number" 0))
                    (set! abstractpart (checkNumListType abstractpart "number" 0))
                    (set! CoeffFunction (lambda (val i) (/ val valcoeff)))
                    (lambda (val) (send this filterPartValue val))
                )
                ((or (equal? abstractType "coord2D") (equal? abstractType "coord3D") (equal? abstractType "colour"))
                    (set! valcoeff (checkNumListType valcoeff "list" (list DEFAULT_VAL_COEFF)))
                    (set! eventpart (checkNumListType eventpart "list" (list 0)))
                    (set! abstractpart (checkNumListType abstractpart "list" (list 0)))
                    (set! CoeffFunction (lambda (val i) (/ val (list-ref valcoeff (min i (length valcoeff))))))
                    (set! AbstractTableParts (initAbstractTableParts))
                    (lambda (val) (send this filterPartValues val))
                )
                ((equal? abstractType "string")
                    (set! eventpart (checkNumListType eventpart "number" 0))
                    ;(set! abstractpart (checkNumListType "number" 0)) ;MultiString AbstractType?
                    (lambda (val) (send this filterString val))
                )
                ((equal? abstractType "Tuio")
                    (lambda (val) (send this filterTuio val))
                )
            )
        )
        (define/private (checkNumListType val type default)
            (cond
                ((equal? type "number")
                    (cond
                        ((number? val) val)
                        ((list? val) (first valcoeff))
                        (else default)
                    )
                )
                ((equal? type "list")
                    (cond
                        ((list? val) val)
                        ((number? val) (list val))
                        (else default)
                    )
                )
                (else default)
            )
        )
        (define/private (initAbstractTableParts)
            (map
                (lambda (TableP AbstractP)
                    (list AbstractP TableP)
                )
                eventpart
                abstractpart
            )
        )
        (define/private (CoeffTypeCheck)
            (cond
                ((number? valcoeff)
                    (set! valcoeff (list valcoeff))
                )
                ((list? valcoeff)
                    #t
                )
                (else
                    (set! valcoeff '(127))
                    #f
                )
            )
        )
        (define/public (filterPartValue value)
            (CoeffFunction (list-ref value (min eventpart (length value))) 0)
        )
        (define/private (filterPartValues TableValues)
        ;Retrieve required values from TableControl value, depending AbstractPart EventPart
        ;Applied to AbstractValues
            (let ((AbstractValue (send Abstract getValue)))
                (vector-map
                    (lambda (i)
                        (let ((AP.TP (assoc i AbstractTableParts)))
                            (cond
                                (AP.TP
                                    (CoeffFunction (list-ref TableValues (second AP.TP)) i)
                                )
                                (else (list-ref AbstractValue i))
                            )
                        )
                    )
                    (range AbstractValue)
                )
            )
        )
        (define/public (filterString value)
            (let ((filterS (list-ref value eventpart)))
                (when (string? filterS)
                    filterS
                )
            )
        )
        (define/public (filterTuio value)
            value                                           ; TODO : Tuio Filtering
        )
        (define/public (updateControl val)
            (send Abstract updateControl (FilterFunction val) tablePlayer)
        )
        (define/public (setPlayer p)
            (send Abstract setPlayer p)
        )
        (define/public (getPlayer)
            tablePlayer
        )
        (super-new)
    )
)
(define TableControl%
    (class* object% (equal<%>)
        (init-field
            type
            address
        )
        (inspect #f)
        (define/public (equal-to? other recur)
            (if
                (and
                    (equal? type (get-field type other))
                    (equal? address (get-field address other))
                )
                #t
                #f
            )

        )
        (define/public (equal-hash-code-of hash-code)
            (+ (hash-code type) (hash-code address))
        )

        (define/public (equal-secondary-hash-code-of hash-code)
            (+ (hash-code type) (hash-code address))
        )
        (define/public (getType)
            type
        )
        (define/public (getAddress)
            address
        )
        (super-new)
    )
)

