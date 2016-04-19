(require
    racket/class
    racket/list
    racket/vector
    racket/string
    racket/base
    json
)
(require
    "modules/vjbros.rkt"
;    "modules/Tuio.ss"
)

(load "source/globals.scm")
(load "source/control.scm")
(load "source/visual.scm")
(load "source/player.scm")
(load "source/controlmapper.scm")
(load "source/mapping.scm")


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

(define TUIO_SOURCE DEFAULT_TUIO_SOURCE)
(define previous_tuio_source_enable #f)
(when (defined? 'tuio-source-enable) (set! previous_tuio_source_enable tuio-source-enable))  ;check for previous session tuio server state
(define tuio-source-enable previous_tuio_source_enable)
(define (tuio-source-launch)
    (when (and (not tuio-source-enable) (equal? TUIO_SOURCE DEFAULT_TUIO_SOURCE))
        (show (string-append "Tuio activated on port " (number->string TUIO_SOURCE)))
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

(define (loadConfig playerList mapper visualnameList visualList mappings)
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
        (smoothing-bias (parseJson '(smoothing-bias) configJson DEFAULT_SMOOTHING_BIAS))
        (set! OSC_SOURCE (parseJson '(OscSource) configJson DEFAULT_OSC_SOURCE))
        (when (parseJson '(interfaceControl) configJson #f) (osc-source-launch))
        (hash-for-each
            playersJson
            (lambda (key val)
                (let ((newPlayer (new Player% (name (symbol->string key)) (controlMapper mapper) (visuVisualControlNamesList visualnameList) (visualList visualList) (mappingManager mappings))))
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
    (* (send id getControl nameV) coefficient)
)

(define VisualList (make-hash))
(define VisualNameList (make-hash))
(define PlayerList (make-hash))
(define MappingManager (new Mapping%))
(define Mapper (new ControlMapper% (Players PlayerList) (Mappings MappingManager)))

(define (controlthread) (send Mapper controlThread))
(define (to-player name)
    (hash-ref PlayerList name)
)

(show "")(show "")(show "")
(show "Executed")
(loadConfig PlayerList Mapper VisualNameList VisualList MappingManager)
