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
            mappingManager
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
                            ((or (equal? typeT "midi-ccn")  (equal? typeT "ccn") (equal? typeT "midi-note"))
                                (eval-string  (parseJson '(address) TableJson "0"))
                            )
                            ((equal? typeT "osc")
                                (parseJson '(address) TableJson "/")
                            )
                            ((equal? typeT "kb")
                                (string-ref (parseJson '(address) TableJson "²") 0)
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
            (show-d nameV)
            (show-d LevelV)
            (let*
                (
                    (typeV (loadVisualControlType visuV nameV))
                    (defaultsV
                        (cond
                            ((equal? typeV "Tuio")
                                (list (list) (list) (list) (list))
                            )
                            ((equal? typeV "number")
                                (let
		                                (
                                        (def (parseJson '(defaults) VisualJson '(1 1 1 1)))
                                    )
                                    (map (lambda (d) (if (string? d) (string->number d) d)) def)
                                )
                            )
                        )
                    )

                    (playerV name)
                    (levelV
                        (cond
                            ((string? LevelV) (string->number LevelV))
                            ((number? LevelV) LevelV)
                        )
                    )
                    (valueV (list-ref defaultsV levelV))
                    (visualV (new VisualControl% (Value valueV) (Player playerV) (Type typeV) (name nameV) (defaultValues defaultsV) (level levelV)))
                )
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
            (show-d "->setVisual")
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
                            (unless (and (not (empty? LevelKeys)) (empty? CrossKeys))
                                (for-each       ; stop and unrecord running visuals in cross-Levels
                                    (lambda (K running)
                                        (let ((visualK (hash-ref visualList K)))
                                            (when running
                                                (send visualK Stop)
                                            )
                                            (send controlMapper unRecordControl (loadTableValuesVisualControls (send visualK getVisual) (send visualK getCrossLevel)) #:force #t)
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
            (show-d "->visualStart")
            (let ((CrossLevel (getCrossLevel cross level)))
                (when CrossLevel
                    (when visual
                        (setVisual visual (getCross cross) (getLevel level))
                    )
                    (cond
                        ((or waitFocus (empty? lastFocus))
                            (setFocus CrossLevel)
                        )
		            )
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
						                            ((>= levIter DEFAULT_OTHER_LEVEL_SEARCH)
					                		            #f
						                            )
						                            (else
					                		            (searchOtherLevel (+ levIter 1))
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
                ((or (not (empty? lastFocus)) cross)
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
            (show-d "->loadTableValuesVisualControls")
            (cond
                ((and (hash-has-key? visuTempTableControl (string-append visu cross)) (not forceReload))
                    (let ((visuTempControls (hash-ref visuTempTableControl (string-append visu cross))))
                        (show-d "->loadTableValuesVisualControls")
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
                                    (let ((inputFilePath (string-append DEFAULT_CONTROL_PATH visu "." player "." "json")))
                                        (when (file-exists? inputFilePath)
                                            (let*
                                                (
                                                    (inputFile (open-input-file inputFilePath))
                                                    (inputJson (read-json inputFile))
                                                )
                                                (hash-for-each
                                                    inputJson
                                                    (lambda (key val)
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
                                                            (when (and (member nameV visualNames) (not (member nameV visualControlsNamesFound)))
                                                                (let*
                                                                    (
                                                                        (TableC (generateTableControl val player))
                                                                        (VisualC (generateVisualControl val nameV visu levelV))
                                                                        (FilterC (generateFilterControl val VisualC))
                                                                    )
                                                                    (when (equal? (send TableC getType) "tuio")
                                                                        (send controlMapper recordControl (list (list TableC FilterC VisualC)))
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
                                            (show-d (string-append "Controls Missing -" (first players)))
                                            (show "Controls Missing")
                                            (for-each (lambda (miss) (show miss)) missingControl)
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
                            (visufiltercontrolList (parseAllPlayerControl))
                        )
                        (hash-set! visuTempTableControl (string-append visu cross) visufiltercontrolList)
                        (show-d "->loadTableValuesVisualControls")
                        visufiltercontrolList
                    )
                )
            )
        )
        (define/private (loadVisualControlsNames visu) ; return '(VisualControlName)
            (hash-keys (loadVisualControlsNamesTypes visu))
        )
        (define/private (loadVisualControlType visu control)
            (show-d "->loadVisualControlType")
            (let*
              (
                (VisualNameTypes   (loadVisualControlsNamesTypes visu))
                (controlType (hash-ref VisualNameTypes control "number"))
              )
              (show-d "  loadVisualControlType->")
              controlType
            )
        )
        ;;; Parse Visual file to found all Controls and associated Types
        (define/private (loadVisualControlsNamesTypes visu) ; return (hash-ref visuVisualControlNamesList visu)
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
        (define/public (saveControls (cross #f) (level #f))
            (show "SaveControls")
            (let ((crosslevel (getCrossLevel cross level)))
                (show crosslevel)
                (when crosslevel
                    (saveCrossLevelControls crosslevel)
                )
            )
        )
        (define/private (saveCrossLevelControls crossLevel)
            (show "->saveCrossLevelControls")
            (when (hash-has-key? visualList crossLevel)
                1
            )
        )
        (define/private (mapControls visu)
            ;(let
            ;    (
            ;        (controlList (loadTableVisualControlList visu))
            ;        (tableControlZone TableZoneList)
            ;    )
                controlList
            ;)
        )
        (define/public (setFocus (crosslevel #f) (forceReload #f) (nFocus 0)) ;unRecordControl[F] & recordControl[F] -> ControlMapper
            (cond
                (crosslevel
                    (unless (empty? lastFocus)
                        (let ((oldLastFocus (string-append (first lastFocus) (second lastFocus))))
                            (when (hash-has-key? visualList oldLastFocus)
	                            (send controlMapper unRecordControl (loadTableValuesVisualControls (send (hash-ref visualList oldLastFocus) getVisual) oldLastFocus))
	                        )
                        )
                    )
                    (show "debug setfocus")
                    (show crosslevel)
                    (show (string? crosslevel))

                    (set! focus (list crosslevel))
                    (set! lastFocus
                        (list
                            (substring crosslevel 0 DEFAULT_CROSS_SIZE)
                            (substring crosslevel DEFAULT_CROSS_SIZE)
                        )
                    )
                    (set! waitFocus #f)
                    (when (hash-has-key? visualList crosslevel)
			(show "Focus visual")
			(show (send (hash-ref visualList crosslevel) getVisual))
			(show "")
                        (send controlMapper recordControl (loadTableValuesVisualControls (send (hash-ref visualList crosslevel) getVisual) crosslevel #:forceReload forceReload))
			;(show (send controlMapper getMap))
                    )
                )
                (else
                    (set! waitFocus #t)
                )
            )
        )
        (define/public (unsetFocus (crosslevel #f))
	    (show "debug unsetFocus")
	    (unless (empty? lastFocus)
		(show "debug 1")
		(let ((oldLastFocus (string-append (first lastFocus) (second lastFocus))))
		    (show "debug 2")
		    (send controlMapper unRecordControl (loadTableValuesVisualControls (send (hash-ref visualList oldLastFocus) getVisual) oldLastFocus))
		    (set! lastFocus '())
		    (show "debug 3")
		)
	    )
	)
        (define/public (setFocus-on-player player)
	  (show "debug")
	  (let
	    (
	      (hash-players (send controlMapper get-players))
	    )
	    (cond
	      ((hash-has-key? hash-players player)
		(show "player found")
		(show (hash-ref hash-players player))
		(let*
		    (
			(hash-player (hash-ref hash-players player))
			(hash-focus (send hash-player getFocus))
		    )
		    (cond
			((empty? hash-focus)
			    (show (string-append "No Focus from " player))
			)
			(else
			    (let*
				(
				    (hash-cross (list-ref hash-focus 0))
				    (hash-level (list-ref hash-focus 1))
				    (hash-crosslevel (string-append hash-cross hash-level))
				    (hash-visual (send hash-player getVisual))
				)
				(show hash-focus)
				(show (list-ref hash-focus 0))
				(show (string? (list-ref hash-focus 0)))
				(show (string-append hash-cross hash-level))
				(show hash-visual)
				(show visualList)
				(send hash-player unsetFocus)
				(show "debug setFocusOn0")
				(send (hash-ref visualList hash-crosslevel) setVisualControls (loadTableValuesVisualControls hash-visual hash-crosslevel #:forceReload #t))
				(show "debug setFocusOn")
				(setFocus (string-append hash-cross hash-level))
				(show (getFocus))
			    )
			)
		    )
		(show "enddebug")
		)
	      )
	      (else
		(show "player not found")
	      )
	    )
	  )
	)
        (define/public (getFocus)
            lastFocus
        )
        (define/public (getFocusString)
	    (let ((focus (getFocus)))
		(cond
		    ((empty? focus)
			""
		    )
		    (else
			(string-append (list-ref focus 0) (list-ref focus 1))
		    )
		)
	    )
	)
        (define/public (getVisual)
	    (show visualList)
	    (send (hash-ref visualList (getFocusString)) getVisual)
	)
	(define/public (getVisualObject)
	    (show (getFocusString))
	    (show (hash-ref visualList (getFocusString)))
	    (hash-ref visualList (getFocusString))
	)
        (define/public (getWaitFocus)
            (show "")
            (show waitFocus)
        )
        (super-new)
    )
)
