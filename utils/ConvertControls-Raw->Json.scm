(require json)
(require racket/path)
(require racket/string)

(current-directory "/home/ted/Sources/git/FluxusBros")

(define (convert inpath)
    (letrec
        (
            (inFile (open-input-file inpath))
            (controlList (make-hash))
            (parseControl
                (lambda ()
                    (let
                        (
                            (FILE (read-line inFile))
                            (PLAYER (read-line inFile))
                            (NAME (read-line inFile))
                            (CONTROL (read-line inFile))
                            (ADDRESS (read-line inFile))
                            (MAPPING (read-line inFile))
                            (ACTMODE (read-line inFile))
                            (DEF1 (read-line inFile))
                            (DEF2 (read-line inFile))
                            (DEF3 (read-line inFile))
                            (DEF4 (read-line inFile))
                        )
                        (cond
                            ((not
                                (or
                                    (eof-object? FILE)
                                    (eof-object? PLAYER)
                                    (eof-object? NAME)
                                    (eof-object? CONTROL)
                                    (eof-object? ADDRESS)
                                    (eof-object? MAPPING)
                                    (eof-object? ACTMODE)
                                    (eof-object? DEF1)
                                    (eof-object? DEF2)
                                    (eof-object? DEF3)
                                    (eof-object? DEF4)
                                )
                            )
                                (hash-set! controlList (string->symbol NAME) (hash 'type CONTROL 'address ADDRESS 'defaults (list (string->number DEF1) (string->number DEF2) (string->number DEF3) (string->number DEF4))))
                                (parseControl)
                            )
                        )
                    )
                )
            )
        )
        (parseControl)
        (close-input-port inFile)
        controlList
    )
)

(define ControlList (directory-list "controls"))
(define excludeControlList '(tennis radiohazard quartertracker gridtracker soundwavetracker))
(define excludeControlListStrings (map (lambda (n) (symbol->string n)) excludeControlList))

(for-each
    (lambda (f)
        (let ((extension (filename-extension f)))
            (when extension
                (when (and (not (equal? extension #"json")) (not (equal? extension #"json~")))
                    (let
                        (
                            (splitFileName (string-split (path->string f) "."))
                        )
                        (when (>= (length splitFileName) 2)
                            (if (member (list-ref splitFileName 0) excludeControlListStrings)
                                (begin (display "Ignored : ") (displayln f))
                                (begin 
                                    (display "Converted : ") (displayln f)
                                    (let*
                                        (
                                            (player (list-ref splitFileName 1))
                                            (visu (list-ref splitFileName 0))
                                            (inPath (string-append "controls/" (path->string f)))
                                            (outPath (string-append "controls/" visu "." player ".json"))
                                            (outFile (open-output-file outPath #:exists 'replace))
                                        )
                                        (write-json (convert inPath) outFile)
                                        (close-output-port outFile)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    ControlList
)
;(displayln (write-json (convert "controls/bully.Korg" "controls") #:encode 'all))
;(write-json (convert "controls/bully.Korg" "controls"))
;(define outFile (open-output-file "bully.Korg.json" #:exists 'replace))
;(write-json (convert "controls/bully.Korg" "controls") outFile #:encode 'all)
;(close-output-port outFile)
