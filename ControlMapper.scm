(require
    racket/class
    racket/list
    racket/vector
    racket/string
    mzlib/string
    json
)
(require
    "modules/vjbros.scm"
    "modules/Tuio.ss"
)

;defaults variables
(define DEFAULT_THRESHOLD_TRIGGER_VALUE 0.5)
(define DEFAULT_VISUAL_PATH "visus/")
(define DEFAULT_CONTROL_PATH "controls/")
(define DEFAULT_CROSS_SIZE 3)
(define DEFAULT_OTHER_LEVEL_SEARCH 4)
(define DEFAULT_VAL_COEFF 127)
(define DEFAULT_HOOK_INTERVAL 0.05)
(define DEFAULT_GAIN 1)

;Configuration OSC
(define DEFAULT_OSC_SOURCE "3334")
(define DEFAULT_OSC_DESTINATION "4444")
(define OSC_SOURCE DEFAULT_OSC_SOURCE)
(define previous_osc_source_enable #f)
(when (defined? 'osc-source-enable) (set! previous_osc_source_enable osc-source-enable))    ;check for previous session osc server state
(define osc-source-enable previous_osc_source_enable)
(define (osc-source-launch)
    (when (and (not osc-source-enable) (equal? OSC_SOURCE DEFAULT_OSC_SOURCE))
        (osc-source OSC_SOURCE)
        (set! osc-source-enable #t)
    )
)

;Configuration TUIO
(define DEFAULT_TUIO_SOURCE 3332)
(define TUIO_SOURCE DEFAULT_TUIO_SOURCE)
(define previous_tuio_source_enable #f)
(when (defined? 'tuio-source-enable) (set! previous_tuio_source_enable tuio-source-enable))  ;check for previous session tuio server state
(define tuio-source-enable previous_tuio_source_enable)
(define (tuio-source-launch)
    (when (and (not tuio-source-enable) (equal? TUIO_SOURCE DEFAULT_TUIO_SOURCE))
        (start-tuio-client #f TUIO_SOURCE)
        (set! tuio-source-enable #t)
    )
)

(define previous-debug #f)
(when (defined? 'debug)
    (set! previous-debug debug)
)
(define debug previous-debug)
(define (show-d t)
    (when debug
        (show t)
    )
)

;System reset
(rm-all-tasks)
(reset-camera)
(clear)
(clear-colour 0)
(blur 0)

(define AbstractControl%
    (class object%
        (super-new)
        (init-field
            Value
            Player
            Type
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
            (if (equal? Player player)
                (setValue val)
                (when (filterFunction val)
                    (set! Player player)
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
        (define/public (getValue)
            (getControl)
        )
        (define/public (getControl)
            Value
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
        (inherit setValue getValue)
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
        (define/public (getName)
            name
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
                        (lambda ()
                            (eval-string "void")
                        )
                    )
                    this
                )
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
                        (lambda ()
                            (eval-string "void")
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
        (super-new)
    )
)

;Trigger Interface
;setVisu
;setFocus
;(re)loadTriggerControls
;(re)loadVisualControls

(define Player-Interface
    (interface ()
        setVisual
        visualStart
    )
)

(define Player%
    (class* object% (Player-Interface)
        (init-field
            name                        ;name of the player
            controlMapper               ;ControlMapper% control signal mapper
            visuVisualControlNamesList
            visualList                   ;Hash[CrossLevel]Visuals%
        )
        (field
            (focus '())
            (waitFocus #f)
            (lastFocus '())
            (focusTable #f)                ; List to store Focus's TableControl Mapping    '( '( '(Zone1-1) '(Zone1-2)) '( '(Zone2-1) '(Zone2-2)))
            (visuTempTableControl (make-hash))       ; List to store TableValuesVisualControl list  hash["name"]'(TableControl DefaultValues VisualControls)
            (defaultDirectory "")
            (triggersControlList (make-hash))
        )
        (define/private (generateTableControl TableJson (player name))
            (show-d "->genTableControl")
            (let*
                (
                    (typeT
                        (if (equal? player name)
                            (parseJson '(type) TableJson "fake")
                            "fake"
                        )
                    )
                    (addressT
                        (cond
                            ((or (equal? typeT "midi-ccn") (equal? typeT "midi-note"))
                                (eval-string  (parseJson '(address) TableJson "0"))
                            )
                            ((equal? typeT "osc")
                                (parseJson '(address) TableJson "/")
                            )
                            (else
                                (eval-string  (parseJson '(address) TableJson "0"))
                            )
                        )
                    )
                    (tableT (new TableControl% (type typeT) (address addressT)))
                )
                (show-d addressT)
                (show-d "  genTableControl->")
                tableT
            )
        )
        (define/private (generateVisualControl VisualJson nameV visuV LevelV)
            (show-d "->genVisualControl")
            (let*
                (
                    (typeV (loadVisualControlType visuV nameV))
                    (defaultsV
                        (cond
                            ((equal? typeV "Tuio")
                                (list (list) (list) (list) (list))
                            )
                            ((equal? typeV "number")
                                (parseJson '(defaults) VisualJson '(1 1 1 1))
                            )
                        )
                    )
                            
                    (valueV 0.555)
                    (playerV #f)
                    (levelV
                        (cond
                            ((string? LevelV) (string->number LevelV))
                            ((number? LevelV) LevelV)
                        )
                    )
                    (visualV (new VisualControl% (Value valueV) (Player playerV) (Type typeV) (name nameV) (defaultValues defaultsV) (level levelV)))
                )
                (send visualV setLevel levelV)
                (show-d "typev")
                (show-d typeV)
                (show-d "  genVisualControl->")
                visualV
            )
        )
        (define/private (generateTriggerControl TriggerJson)
            (show-d "->genTriggerControl")
            (let*
                (
                    (rawTrigg->Trigg
                        (lambda (t)
                            (cond
                                (t
                                    (string-append "(send (to-player \"" name "\") " t ")")
                                )
                                (else "")
                            )
                        )
                    )
                    (ontriggRaw (parseJson '(On) TriggerJson #f))
                    (offtriggRaw (parseJson '(Off) TriggerJson #f))
                    (onTriggT (rawTrigg->Trigg ontriggRaw))
                    (offTriggT (rawTrigg->Trigg offtriggRaw))
                    (valueT 0)
                    (playerT name)
                    (typeT "number")
                    (triggerT (new TriggerControl% (Type typeT) (Player playerT) (Value valueT) (onTrigg onTriggT) (offTrigg offTriggT)))
                )
                (show-d "  genTriggerControl->")
                triggerT
            )
        )
        (define/private (generateFilterControl FilterJson AbstractF)
            (show-d "->genFilterControl")
            (let*
                (
                    (playerF name)
                    (eventpartF (parseJson '(tablePart) FilterJson 0))
                    (valcoeffF (parseJson '(valCoeff) FilterJson DEFAULT_VAL_COEFF))
                    (assignF (parseJson '(assignement) FilterJson "assign"))
                    (abstractpartF (parseJson '(abstractPart) FilterJson 0))
                    (filterF (new FilterControl% (Abstract AbstractF) (tablePlayer playerF) (eventpart eventpartF) (valcoeff valcoeffF) (assign assignF) (abstractpart abstractpartF)))
                )
                (show-d "  genFilterControl->")
                filterF
            )
        )
        (define/private (loadDefaultDirectory (defaultDir name))
            (if (directory-exists? defaultDir)
                (set! defaultDirectory defaultDir)
                ""
            )
        )
        (define/private (filePath jsonFile)
            (let ((path (string-append defaultDirectory jsonFile ".json")))
                (if (file-exists? path)
                    path
                    #f
                )
            )
        )
        (define/private (filesPath jsonFiles)
            (let ((result '()))
                (for-each
                    (lambda (f)
                        (let*
                            (
                                (fs
                                    (cond
                                        ((symbol? f)
                                            (symbol->string f)
                                        )
                                        ((string? f)
                                            f
                                        )
                                        (else "")
                                    )
                                )
                                (fpath (filePath fs))
                            )
                            (when fpath (set! result (append result (list fpath))))
                        )
                    )
                    jsonFiles
                )
                result
            )
        )
        (define/public (loadPlayerConfig config) ;loadConfigFile -> loadVisu -> loadMappings -> ???
            (let*
                (
                    (defaultDir (parseJson '(defaultDirectory) config))
                    (configTriggers (parseJson '(loadTriggers * true) config))
                    (configVisuals (parseJson '(loadVisuals * true) config))
                )
                (loadDefaultDirectory defaultDir)
                (loadTriggerControls (filesPath configTriggers))
            )    
        )
        (define/public (loadTriggerControls files)
            (for-each
                (lambda (f)
                    (show-d f)
                    (let*
                        (
                            (jsonFile (open-input-file f))
                            (jsonHash (read-json jsonFile))
                        )
                        (hash-for-each
                            jsonHash
                            (lambda (key val)
                                (let*
                                    (
                                        (newTable  (generateTableControl val))
                                        (newTrigg  (generateTriggerControl val))
                                        (newFilter (generateFilterControl val newTrigg))
                                    )
                                    (hash-set! triggersControlList key newTrigg)
                                    (send controlMapper recordControl (list (list newTable newFilter)))
                                )
                            )
                        )
                        (close-input-port jsonFile)
                    )
                )
                files
            )
        )
        (define/public (setVisual visual (cross #f) (level #f)) ;loadVisu defaultValue
            (show "->setVisual")
            (let
                (
                    (CrossLevel (getCrossLevel cross level))
                    (visu
                        (cond
                            ((string? visual) visual)
                            ((symbol? visual) (symbol->string visual))
                        )
                    )
                )
                (show lastFocus)
                (show CrossLevel)
                (cond
                    ((and CrossLevel (file-exists? (string-append DEFAULT_VISUAL_PATH visu ".scm")))
                        (let*
                            (
                                (LevelKeys (filter (lambda (key) (equal? CrossLevel (substring key 0 (string-length CrossLevel)))) (hash-keys visualList))) ; '(CrossKeys) existing Visual% for Cross
                                (CrossKeys
                                    (cond
                                        ((not (empty? LevelKeys))
                                            (if (equal? visu (send (hash-ref visualList (first LevelKeys)) getVisual))
                                                '()
                                                LevelKeys
                                            )
                                        )
                                        (else '())
                                    )
                                )
                                (AllCrossKeys (if (member CrossLevel CrossKeys) CrossKeys (append (list CrossLevel) CrossKeys)))
                                (Visuals (map (lambda (key) (hash-ref visualList key)) CrossKeys))
                                (VisualsRunning? (map (lambda (V) (if (send V running?) #t #f)) Visuals))
                            )
                            (show LevelKeys)
                            (show CrossKeys)
                            (unless (and (not (empty? LevelKeys)) (empty? CrossKeys))
                                (for-each       ; stop and unrecord running visuals in cross-Levels
                                    (lambda (K running)
                                        (let ((visualK (hash-ref visualList K)))
                                            (when running
                                                (send visualK Stop)
                                            )
                                (show "debug setVisual")
;;                                 (show visualK)
;;                                 (show (send visualK getVisual))
;;                                 (show (send visualK getCrossLevel))
                                            (send controlMapper unRecordControl (loadTableValuesVisualControls (send visualK getVisual) (send visualK getCrossLevel)))
                                        )
                                    )
                                    CrossKeys VisualsRunning?
                                )
                                (for-each
                                    (lambda (key)
                                        (when (and (member key focus) (hash-has-key? visuTempTableControl key))
                                            (hash-remove! visuTempTableControl key)                                                   ; remove visuTemp if crossLevel in Focus[]
                                        )
                                    )
                                    CrossKeys
                                )
                                (for-each       ;assign new Visual% to crossLevels
                                    (lambda (key)
                                        (let*
                                            (
                                                (tablevisuControlList (loadTableValuesVisualControls visu key))
                                            )
                                            (hash-set! visualList key (addVisual visu CrossLevel))                    ; assign new Visual% to crossLevels
                                            (send (hash-ref visualList key) setVisualControls tablevisuControlList)   ; assign VisualControls% to Visual
                                            (when (member key focus)
                                                (send Mapper recordControl tablevisuControlList)
                                            )
                                            (when (empty? focus)
                                                (when (equal? key CrossLevel)
                                                    (setFocus CrossLevel)
                                                )
                                            )
                                        )
                                    )
                                    AllCrossKeys
                                )
                                (for-each
                                    (lambda (key run)
                                        (when run
                                            (send (hash-ref visualList key) Start)
                                        )
                                    )
                                    CrossKeys VisualsRunning?
                                )
                                ; TODO (for-each CrossKeys (send controlMapper recordControl (mapControls (loadTableControl visu))))
                            )
                        )
                    )
                    (else
                        (when (file-exists? (string-append DEFAULT_VISUAL_PATH visu ".scm"))
                            (loadVisualControl visu)
                        )
                    )
                )
            )
            (show-d "  setVisual->")
            #f
        )
        (define/public (visualStart (cross #f) (level #f) (visual #f))
            (let ((CrossLevel (getCrossLevel cross level)))
                (when CrossLevel
                    (when visual
                        (setVisual (getCross cross) (getLevel level) visual)
                    )
                    (cond
                        (waitFocus
                            (setFocus CrossLevel)
                        )
                        (else
                            (cond
                                ((hash-has-key? visualList CrossLevel)
                                    (send (hash-ref visualList CrossLevel) Start)
                                )
                                (else
                                    (letrec
                                        (
                                            (searchOtherLevel
                                                (lambda ((levIter 0))
                                                    (let ((lev (number->string levIter)))
                                                        (cond
                                                            ((hash-has-key? visualList (getCrossLevel cross lev))
                                                                (setVisual (send (hash-ref visualList (getCrossLevel cross lev)) getVisual) cross level)
                                                                (visualStart cross level)
                                                            )
                                                            ((>= n DEFAULT_OTHER_LEVEL_SEARCH)
                                                                #f
                                                            )
                                                            (else
                                                                (searchOtherLevel (+ lev 1))
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                        (searchOtherLevel)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        (define/public (visualStop (cross #f) (level "0"))
            (let ((CrossLevel (getCrossLevel cross level)))
                (when CrossLevel
                    (when (hash-has-key? visualList CrossLevel)
                        (send (hash-ref visualList CrossLevel) Stop)
                    )
                )
            )
        )
        (define/private (addVisual v c)
            (new Visual% (name v) (crosslevel c))
        )
        (define/private (getCrossLevel cross level)
            (cond
                ((or lastFocus cross)
                    (let*
                        (
                            (autocrossfader (getCross cross))
                            (autolevel (getLevel level))
                            (autocrosslevel (string-append autocrossfader autolevel))
                        )
                        autocrosslevel
                    )
                )
                (else #f)
            )
        )
        (define/private (getCross cross)
            (if cross
                (let
                    (
                        (crossfader
                            (cond
                                ((number? cross) (number->string cross))
                                ((symbol? cross) (symbol->string cross))
                                ((string? cross) cross)
                                (else #f)
                            )
                        )
                    )
                    (string-append (build-string (- DEFAULT_CROSS_SIZE (string-length crossfader)) (lambda (i) #\0)) crossfader) ;Add 0's in front of cross number when cross < 100
                )
                (if (empty? lastFocus)
                    #f
                    (first lastFocus)
                )
            )
        )
        (define/private (getLevel level)
            (cond
                (level
                    (cond
                        ((string? level)
                            level
                        )
                        ((symbol? level)
                            (symbol->string level)
                        )
                        ((number? level)
                            (number->string level)
                        )
                    )
                )
                (else
                    (cond
                        ((not (empty? lastFocus))
                            (second lastFocus)
                        )
                        (else
                            "0"
                        )
                    )
                )
            )
        )
        (define/private (loadTableValuesVisualControls visu cross #:forceReload (forceReload #f)) ; return '(TableControl% VisualControl%)
            (cond
                ((and (hash-has-key? visuTempTableControl cross) (not forceReload))
                    (let ((visuTempControls (hash-ref visuTempTableControl visu)))
;;                         (when record?
;;                             (hash-for-each
;;                                 visuTempControls
;;                                 (lambda (TableKey FilterVisualVal)
;;                                     (send controlMapper recordControl (list TableKey (first FilterVisualVal)))
;;                                 )
;;                             )
;;                         )
                        visuTempControls
                    )
                )
                (else
                    (letrec
                        (
                            (Cross (substring cross 0 DEFAULT_CROSS_SIZE))
                            (Level (substring cross DEFAULT_CROSS_SIZE))
                            (visualControlsNames (loadVisualControlsNames visu))
                            (visualControlsNamesFound '())
                            (parseControlFileJson ; return '(TableControl% VisualControl%) from file and ('(VisualControlNames) | '())
                                (lambda (player visualNames (tablevisualList '()))
                                    (show-d "->parseControlFileJson")
                                    (let ((inputFilePath (string-append DEFAULT_CONTROL_PATH visu "." player "." "json"))) ; Next - change to (string-append DEFAULT_CONTROL_PATH visu ".xml"
                                        (when (file-exists? inputFilePath)
                                            (let*
                                                (
                                                    (inputFile (open-input-file inputFilePath))
                                                    (inputJson (read-json inputFile))
                                                )
                                                (hash-for-each
                                                    inputJson
                                                    (lambda (key val)
                                                        ;(show-d "->parseControlFileJson hash-for-each")
                                                        (let
                                                            (
                                                                (nameV
                                                                    (cond
                                                                        ((string? key) key)
                                                                        ((symbol? key) (symbol->string key))
                                                                        ((number? key) (number->string key))
                                                                    )
                                                                )
                                                                (levelV Level)
                                                            )
                                                            (show-d nameV)
;;                                                             (show-d (string? nameV))
;;                                                             (show-d visualNames)
;;                                                             (show-d levelV)
;;                                                             (show-d record?)
                                                            (when (and (member nameV visualNames) (not (member nameV visualControlsNamesFound)))
;;                                                                 (cond
;;                                                                     (record?
;;                                                                         (let*
;;                                                                             (
;;                                                                                 (TableC (generateTableControl val player))
;;                                                                                 (VisualC (generateVisualControl val nameV visu levelV))
;;                                                                                 (FilterC (generateFilterControl val VisualC))
;;                                                                             )
;; ;;                                                                             (when record?
;; ;;                                                                                 (send controlMapper recordControl (list (list TableC FilterC)))
;; ;;                                                                             )
;;                                                                             (set! tablevisualList (append tablevisualList (list (list TableC VisualC))))
;;                                                                         )
;;                                                                     )
;;                                                                     (else
;;                                                                         (let*
;;                                                                             (
;;                                                                                 (VisualC (generateVisualControl VisualJson nameV visu levelV))
;;                                                                             )
;;                                                                             (set! tablevisualList (append tablevisualList (list (list #f visualC))))
;;                                                                         )
;;                                                                     )
;;                                                                 )
                                                                (let*
                                                                    (
                                                                        (TableC (generateTableControl val player))
                                                                        (VisualC (generateVisualControl val nameV visu levelV))
                                                                        (FilterC (generateFilterControl val VisualC))
                                                                    )
                                                                    (set! tablevisualList (append tablevisualList (list (list TableC FilterC VisualC))))
                                                                )
                                                                (set! visualControlsNamesFound (append visualControlsNamesFound (list nameV)))
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                    (show-d "parseControlFileJson->")
                                    tablevisualList
                                )
                            )
                            (searchOtherPlayer ; return available '(players) for visual controls with Player%.name first
                                (lambda ()
                                    (let*
                                        (
                                            (dirFiles (map (lambda (i) (path->string i)) (directory-list DEFAULT_CONTROL_PATH)))
                                            (fileStringParse (string-append visu "."))
                                            (filterLengthFiles (filter (lambda (f) (>= (string-length f) (+ (string-length visu) 1))) dirFiles))
                                            (allPlayerFiles (filter (lambda (f) (equal? fileStringParse (substring f 0 (+ (string-length visu) 1)))) filterLengthFiles))
                                            (filterPlayerFiles (remove (string-append visu "." name) allPlayerFiles))
                                            (filterPlayers (map (lambda (f) (substring f (+ (string-length visu) 1))) filterPlayerFiles))
                                            (filterAllPlayers (append (list name) filterPlayers))
                                        )
                                        filterAllPlayers
                                    )
                                )
                            )
                            (parseAllPlayerControl
                                (lambda ((tablevisuList '()) (players (searchOtherPlayer)) (playersIter 0) (missingControl visualControlsNames))
                                    (cond
                                        ((empty? missingControl)
                                            tablevisuList
                                        )
                                        ((>= playersIter (length players))
                                            (show-d (string-append "Controls Missing -" players))
                                            tablevisuList
                                        )
                                        (else
                                            (let*
                                                (
                                                    (objectsList (parseControlFileJson (list-ref players playersIter) missingControl))
                                                    (notfoundNames (remove* visualControlsNamesFound missingControl))
                                                )
                                                (set! visualControlsNamesFound '())
                                                (parseAllPlayerControl (append tablevisuList objectsList) players (+ playersIter 1) notfoundNames)
                                            )
                                        )
                                    )
                                )
                            )
                        )
                        (parseAllPlayerControl)
                    )
                )
            )
        )
        (define/private (loadVisualControlsNames visu) ;return '(VisualControlName)
            (hash-keys (loadVisualControlsNamesTypes visu))
        )
        (define/private (loadVisualControlType visu control)
            (show-d "->loadVisualControlType")
            (show-d (loadVisualControlsNamesTypes visu))
            (hash-ref (loadVisualControlsNamesTypes visu) control "number")
        )
        (define/private (loadVisualControlsNamesTypes visu)
            (cond
                ((hash-has-key? visuVisualControlNamesList visu)
                    (hash-ref visuVisualControlNamesList visu)
                )
                (else
                    (hash-set! visuVisualControlNamesList visu (make-hash))
                    (letrec
                        (
                            (inFilePath (string-append DEFAULT_VISUAL_PATH visu ".scm"))
                            (inFile (open-input-file inFilePath #:mode 'text))
                            (controlPattern "\\(c \".*?\" id.*?\\){1}")
                            (namePattern "(?<=\\(c \")[\\w+\\-*]+(?=\")")
                            (typePattern "(?<=\\#\\:type \")[\\w+]+(?=\")")
                            (inFileParse
                                (lambda ((inLine (read-line inFile)))
                                    (unless (eof-object? inLine)
                                        (for-each
                                            (lambda (ctrl)
                                                (let*
                                                    (
                                                        (nameCtl (regexp-match (pregexp namePattern) ctrl))
                                                        (typeC (regexp-match (pregexp typePattern) ctrl))
                                                        (typeCtl
                                                            (if typeC
                                                                (first typeC)
                                                                "number"
                                                            )
                                                        )
                                                    )
                                                    (when (not (empty? nameCtl))
                                                        (hash-set!
                                                            (hash-ref visuVisualControlNamesList visu) 
                                                            (first nameCtl)
                                                            typeCtl
                                                        )
                                                    )
                                                )
                                            )
                                            (regexp-match* (pregexp controlPattern) inLine)
                                        )
                                        (inFileParse)
                                    )
                                )
                            )
                        )
                        (file-position inFile 0)
                        (inFileParse)
                        (close-input-port inFile)
                        (load inFilePath)
                        (hash-ref visuVisualControlNamesList visu)
                    )
                )
            )
        )
        (define/private (mapControls visu)
            (let
                (
                    (controlList (loadTableVisualControlList visu))
                    (tableControlZone TableZoneList)
                )
                controlList
            )
        )
        (define/public (setFocus (crosslevel #f) (nFocus 0)) ;unRecordControl[F] & recordControl[F] -> ControlMapper
            (show "setFocus")
            (cond
                (crosslevel
                    (unless (empty? lastFocus)
                        (let ((oldLastFocus (string-append (first lastFocus) (second lastFocus))))
                            (send controlMapper unRecordControl (loadTableValuesVisualControls (send (hash-ref visualList oldLastFocus) getVisual) oldLastFocus))
                        )
                    )
                    (set! focus (list crosslevel))
                    (set! lastFocus
                        (list
                            (substring crosslevel 0 DEFAULT_CROSS_SIZE)
                            (substring crosslevel DEFAULT_CROSS_SIZE)
                        )
                    )
                    (set! waitFocus #f)
                    (when (hash-has-key? visualList crosslevel)
                        (send controlMapper recordControl (loadTableValuesVisualControls (send (hash-ref visualList crosslevel) getVisual) crosslevel))
                    )
                )
                (else
                    (set! waitFocus #t)
                )
            )
        )
        (define/public (getFocus)
            (show "")(show "")(show "")
            (show lastFocus)
            (show focus)
            lastFocus
        )
        (define/public (getWaitFocus)
            (show "")
            (show waitFocus)
        )
        (super-new)
    )
)

(define ControlMapper%
    (class object%
        (field
            (Map (make-hash))            ;Hash#[TableControl%] '(player FilterControl%)
            (numTypesEvents (make-hash)) ;Hash#[Type]numControl
            (TypesEvents (make-hash))    ;Hash#[Type]funct - for controlThread Queue
            (EventThread (launchThread))

            (KbDown '())                ;'(Keys-Down) buffer save for keys up parsing
            (last-osc-peek "no message yet...")
        )
        (define/public (recordControl table-filterControls) ; table-filterControls '(TableControl% FilterControl%))
            (show-d "->recordControl ")
            (for-each
                (lambda (table-filterControl)
                    (unless (hash-has-key? Map (first table-filterControl))  ; add a new control if no previous control recorded ; TODO : multi '(VisualControl% TriggerControls%) destination
                        (hash-set! Map (first table-filterControl) (second table-filterControl))
                        (setTypeCounts (send (first table-filterControl) getType) 1)
                    )
                )
                table-filterControls
            )
            (show-d "  recordControl->")
        )
        (define/public (unRecordControl tableControls) ; tableControls '(TableControl%) ; TODO : send VisualControl setPlayer #f
            (for-each
                (lambda (tableControl)
                    (when (hash-has-key? Map (first tableControl))
                        (send (hash-ref Map (first tableControl)) setPlayer #f)
                        (hash-remove! Map (first tableControl))
                        (setTypeCounts (send (first tableControl) getType) -1)
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
                1
                (set! KbDown newDown)
            )
        )
        (define/private (kbsEvents)
            1
        )
        (define/private (ccEvents)
            (let ((e (midi-cc-event)))
                (when e
                    (let
                        (
                            (Table (new TableControl% (type "midi-ccn") (address  (vector-take e 2))))
                            (Value (vector-ref e 2))
                        )
                        (updateVisualControl Table (list Value))
                    )
                    (ccEvents)
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
                                ;(updateVisualControl newOscControl msg-peek (updateOscControl newOscControl msg-peek))
                                (convert-msg)
                                (updateVisualControl newOscControl msg-convert)
                            )
                            (set! last-osc-peek peek)
                            (oscEvents)
                        )
                    )
                )
            )
        )
        (define/private (tuioEvents)
            (let*
                (
                    (peekCursor (get-tuio-cursors))
                    (newTuioControl (new TableControl% (type "tuio") (address #t)))
                )
                (updateVisualControl newTuioControl peekCursor)
            )
        )
        (define/private (updateVisualControl tableC Val (notfound-funct (lambda (C) (void))))
            (let ((match (hash-ref Map tableC #f)))
                (cond
                    (match
                        (send match updateControl Val)
                    )
                    (else
                        (notfound-funct tableC)
                    )
                )
            )
        )
        (super-new)
    )
)

(define (parseJson parseList json (not_found '()))
    (if (empty? parseList)
        json
        (if (hash? json)
            (if (hash-has-key? json (first parseList))
                (parseJson (rest parseList) (hash-ref json (first parseList)))
                (cond
                    ((equal? (first parseList) '*)
                        (let ((sub-result '()))
                            (hash-for-each
                                json
                                (lambda (key val)
                                    (when (parseJson (rest parseList) val)
                                        (set! sub-result (append sub-result (list key)))
                                    )
                                )
                            )
                            sub-result
                        )
                    )
                    ((list? (first parseList))
                        (let ((sub-result (make-hash)))
                            (for-each
                                (lambda (key)
                                    (let ((sub-parse (parseJson (append (list key) (rest parseList)) json)))
                                        (when sub-parse
                                            (hash-set! sub-result key sub-parse)
                                        )
                                    )
                                )
                                (first parseList)
                            )
                            sub-result
                        )
                    )
                    (else not_found)
                )
            )
            json
        )
    )
)

(define (loadConfig playerList mapper visualnameList visualList)
    (let*
        (
            (configFile (open-input-file "config.json"))
            (configJson (read-json configFile))
            (playersToLoad (parseJson '(loadPlayers * true) configJson))
            (playersJson (parseJson (list 'players playersToLoad) configJson))
            (audioConf (parseJson '(audio) configJson '("system:capture_1" 1024 44100)))
            (midiInPorts '())
            (midiOutPorts '())
        )
        (start-audio (list-ref audioConf 0) (list-ref audioConf 1) (list-ref audioConf 2))
        (set-gain! (parseJson '(gain) configJson DEFAULT_GAIN))
        (midiin-open 0)
        (set! OSC_SOURCE (parseJson '(OscSource) configJson DEFAULT_OSC_SOURCE))
        (when (parseJson '(interfaceControl) configJson #f) (osc-source-launch))
        (hash-for-each
            playersJson
            (lambda (key val)
                (let ((newPlayer (new Player% (name (symbol->string key)) (controlMapper mapper) (visuVisualControlNamesList visualnameList) (visualList visualList))))
                    (send newPlayer loadPlayerConfig val)
                    (hash-set! playerList (symbol->string key) newPlayer)
                )
                (set! midiInPorts (append midiInPorts (list (parseJson '(MidiInputPort * true) val '()))))
                (set! midiOutPorts (append midiOutPorts (list (parseJson '(MidiInputPort * true) val '()))))
            )
        )
        (cond
            ((= (length midiInPorts) 1)
                ;(midiin-open (first midi-info) (first midiInPorts)))
                (show-d midiInPorts))
            ((> (length midiInPorts) 1)
                (midiin-open 0))
            ((= (length midiOutPorts) 1)
                ;(midiout-open (second (midi-info)) (first midiOutPorts)))
                (show-d midiOutPorts))
            ((> (length midiInPorts) 1)
                (midiout-open 0))
        )
        (close-input-port configFile)
    )
)
                
(define (c nameV id #:type (type 'linear) #:coeff (coefficient 1) #:toggle (toggle #f))
    (send id getControl nameV) 
)

(define VisualList (make-hash))
(define VisualNameList (make-hash))
(define PlayerList (make-hash))
(define Mapper (new ControlMapper%))

(define (controlthread) (send Mapper controlThread))
(define (to-player name)
    (hash-ref PlayerList name)
)

(show "")(show "")(show "")
(show "Executed")
(loadConfig PlayerList Mapper VisualNameList VisualList)

(send (hash-ref PlayerList "Korg") setVisual "radiohazard" "901")
;(send (hash-ref PlayerList "Korg") setVisual "sadray" "902")
(send (hash-ref PlayerList "Korg") visualStart)

;(send (hash-ref PlayerList "FingerPlay") setVisual "tennis" "901")
;(send (hash-ref PlayerList "FingerPlay") visualStart)

;(send (hash-ref PlayerList "Bitstream") setVisual "radiohazard" "902")
;(send (hash-ref PlayerList "Bitstream") visualStart)
;(send (hash-ref PlayerList "Bitstream") setFocus "901" "0")
;(clear-colour (vector 0.2 0.2 0.2))
