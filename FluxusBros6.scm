;----- FluxusBros v6 -------
; TODOS
; (wip) fix : gain task (supprimer les set-gain des visu et ajouter (gh2 * g)
;       fix : visu multi players
;       fix : set-focus sur Crossfader%
;       fix : midi-connect avec VMX
;       implement : fade in/out sur cross -> opacity
;       implement : auto-load visu dans banks
;       implement : trigger lock visu on (touches noires clavier)
;       implement : mapping visual surface
; (nok) implement : gogol
; (ok)  fix : enlever lock-controls sur visu-stop
; (ok)  fix : fonction beat-catch
; (ok)  implement : save-controls

;(current-directory "/Users/TchernoBob/Fluxus/FluxusBros6")
(current-directory "/home/ted/Fluxus/FluxusBros6")

(require "vjbros.scm")
(require scheme/class)
(require mzlib/string)
(require scheme/bool)
(require scheme/list)

(rm-all-tasks)
(reset-camera)
(clear)
(clear-colour 0)
(blur 0)
(smoothing-bias 0.7)

(define normal-var #t)
(define (show-n text . other)
;(show text)
    (when normal-var
        (show text other)
    )
)
(define debug-var #f)
(define (show-d text . other)
    (when debug-var
        (show text other)
    )
)
(define debug-var2 #f)
(define (show-d2 text . other)
    (when debug-var2
        (show text other)
    )
)

(define Crossfaders-Interface
    (interface ()
        set-focus
        set-focus-on-player
        set-crossfader-visu
        set-crossfader-launch
        set-crossfader-stop
        get-control
;        save-controls
        save-controls-on-focus
    )
)

;Crossfaders-public-interface
; set-focus*
; set-crossfader-visu*
; set-crossfader-launch*
; set-crossfader-stop*
; get-control
; save-controls
; save-controls-of-focused
; set-crossfader-visu-mode-default

;Crossfaders-private-interface
; crossfader-check*
; crossfader-create/
; crossfader-remove/
; set-crossfader-to-locked
; set-crossfader-to-unlocked
; set-crossfader-access/
; get-crossfader-access*
; get-crossfader*
; get-crossfader-from-player*
; set-crossfader-player*
; unset-crossfader-player*
; get-crossfader-from-id

(define Crossfaders%
    (class* object% (Crossfaders-Interface)
        (field
            (crossfader-list (make-hash))
            (wait-focus (make-hash))
            (owners (make-hash))
            (players (make-hash))
        )
        (define/public (get-control name id)        ; recuperation de la valeur du control - appel get-control du crossfader
;(show-d "debug get-control entry")
;(show-d id)
;(show-d name)
            (send (get-crossfader-from-id id) get-control name id)
        )
        (define/private (get-crossfader-from-id id)     ; recuperation du crossfader correspondant a l'Id du visu
;(show-d "debug get-crossfader-from-id entry")
;(show-d (substring id 0 3))
            (get-crossfader (string->number (substring id 0 3)))
        )
        (define/public (set-focus #:player player #:crossfader (cross #f) #:bank (bank #f) #:level (level #f))     ; attribution du focus au crossfader/level
            (cond
                ((crossfader-check #:crossfader cross #:player player #:bank bank)
                    (set-crossfader-player #:player player #:crossfader cross #:level level)
                )
                (else
                    (hash-set! wait-focus player #t)
                )
            )
        )
        (define/public (set-focus-on-player #:player player #:player-target player-target)
(show-n "debug set-focus-on-player entry")
            (cond
                ((crossfader-check #:crossfader (get-crossfader-from-player #:player player-target) #:player player-target)
(show-n "debug set-focus-on-player crossfader-check")
                    (set-crossfader-player
                        #:player player
                        #:crossfader (get-crossfader-from-player #:player player-target)
                        #:level (send (get-crossfader (get-crossfader-from-player #:player player-target)) crossfader-get-player-level #:player player-target)
                    )
(show-n (get-crossfader-from-player #:player player))
                )
            )
        )
        (define/private (set-crossfader-player #:player player #:crossfader cross #:level (level #f))        ; ajoute le player au crossfader
(show-n "debug set-crossfader-player entry")
            (let ((old-cross (get-crossfader-from-player #:player player)))
                (when old-cross
                    (unset-crossfader-player #:player player #:crossfader old-cross #:level level)
                )
                (hash-set! players player cross)
                (send (get-crossfader cross) crossfader-set-player #:player player #:level level)
                (hash-set! wait-focus player #f)
            )
        )
        (define/private (unset-crossfader-player #:player player #:crossfader cross #:level (level #f))
(show-d "debug unset-crossfader-player entry")
            (send (get-crossfader cross) crossfader-unset-player #:player player)
            (hash-remove! players player)
        )
        (define/private (get-crossfader-from-player #:player player #:crossfader (cross #f))
            (cond
                ((not cross)
                    (hash-ref players player #f)
                )
                (else
                    cross
                )
            )
        )
        (define/private (get-crossfader cross)
            (hash-ref crossfader-list cross #f)
        )
        (define/private (crossfader-check #:crossfader crossfader #:player player #:bank (bank #f))
(show-d "debug crossfader-check entry")
            (cond
                (crossfader
                    (cond
                        ((hash-has-key? crossfader-list crossfader)
(show-d "debug crossfader-check crossfader-founded")
                            #t
                        )
                        (else
(show-d "debug crossfader-check else")
                            (crossfader-create #:crossfader crossfader #:player player #:bank bank)
(show-d "debug crossfader-check create")
                            #t
                        )
                    )
                )
                (else
                    #f
                )
            )
        )
        (define/private (set-crossfader-access #:crossfader cross #:player player)
            (hash-set! owners player (append (remove cross (hash-ref owners player '())) (list cross)))
        )
        (define/private (get-crossfader-access #:crossfader cross #:player player)
            (find cross (hash-ref owners player '()))
        )
        (define/public (set-crossfader-launch #:visu (visu (void)) #:crossfader (cross #f) #:player player #:mode (mode #f) #:velocity (velocity 1) #:level (level #f) #:bank (bank #f) #:auto (auto #f) #:swap (swap #f))
(show-d "debug set-crossfader-launch entry")
            (let ((crossfader (get-crossfader-from-player #:crossfader cross #:player player)))
                (unless (crossfader-check #:crossfader crossfader #:player player #:bank bank)
(show-d "debug crossfader-visu-launch crossfader-create")
                    (crossfader-create #:crossfader crossfader #:player player #:bank bank)
                )
                (cond
                    ((get-crossfader-access #:crossfader crossfader #:player player)
(show-d "debug crossfader-visu-launch crossfader-access")
                        (when (and (not (eq? visu (void))) (not (send (get-crossfader crossfader) crossfader-get-visu)))
                            (set-crossfader-visu #:crossfader cross #:visu visu #:mode mode #:velocity velocity #:player player #:auto auto #:swap swap)
(show-d "debug crossfader-visu-launch set-crossfader-visu passed")
                        )
                        (send (get-crossfader crossfader) crossfader-visu-launch #:player player #:bank bank #:level level #:swap swap)
(show-d "debug crossfader-visu-launch passed")
                        (when (eq? mode -1)
                            (set-crossfader-player #:crossfader crossfader #:player player)
                        )
(show-d "debug crossfader-visu-launch set-crossfader-player -1")
                        (when (hash-ref wait-focus player #f)
                            (set-crossfader-player #:crossfader crossfader #:player player #:level level)
                        )
(show-d "debug crossfader-visu-launch set-crossfader-player wait-focus")
                    )
                )
            )
        )
        (define/public (set-crossfader-stop #:crossfader (cross #f) #:player player #:level (level #f) #:bank (bank #f) #:force (force #f))
            (let ((crossfader (get-crossfader-from-player #:crossfader cross #:player player)))
                (cond
                    ((crossfader-check #:crossfader crossfader #:player player #:bank bank)
                        (when (get-crossfader-access #:crossfader crossfader #:player player)
                            (send (get-crossfader crossfader) crossfader-visu-stop #:player player #:bank bank #:level level #:force force)
                        )
                    )
                )
            )
        )
        (define/private (crossfader-create #:crossfader cross #:player player #:bank (bank #f))
(show-d "debug crossfader-create entry")
            (cond
                (bank
                    (hash-set! crossfader-list cross (new Crossfader-Bank% (num (number->string cross)) (owner player)))
                    (set-crossfader-access #:player player #:crossfader cross)
                )
                (else
                    (hash-set! crossfader-list cross (new Crossfader% (num (number->string cross)) (owner player)))
                    (set-crossfader-access #:player player #:crossfader cross)
                )
            )
            cross
        )
        (define/public (set-crossfader-visu #:visu visu #:crossfader (crossfader #f) #:player player #:mode (mode 1) #:velocity (velocity 1) #:auto (auto #f) #:swap (swap #f))
(show-d "debug set-crossfader-visu entry")
            (let
                (
                    (cross
                        (cond
                            (crossfader
                                crossfader
                            )
                            (else
                                (get-crossfader-from-player #:player player)
                            )
                        )
                    )
                )
(show-d "debug set-crossfader-visu cross")
(show-d cross)
                (when cross
(show-d "debug set-crossfader-visu cross found")
                    (send (get-crossfader cross) crossfader-set-visu #:player player #:visu visu #:mode mode #:auto auto #:swap swap)
(show-d "debug set-crossfader-visu cross crossfader-set-visu")
                )
            )
        )
        (define/public (save-controls-on-focus #:player player)
            (let ((cross (get-crossfader-from-player #:player player)))
                (when cross
                    (send (get-crossfader cross) crossfader-save-controls #:player player)
                )
            )
        )
        (super-new)
    )
)

;Crossfader-public-interface
; get-control
; crossfader-set-visu*
; crossfader-visu-launch*
; crossfader-visu-stop*
; crossfader-set-player*
; crossfader-unset-player*
; crossfader-set-visu-to-locked*
; crossfader-set-visu-to-unlocked*
; save-controls
; crossfader-get-visu
; crossfader-get-player-level

;Crossfader-private-interface


(define Crossfader-Interface
    (interface ()
        get-control
        crossfader-set-visu
        crossfader-visu-launch
        crossfader-visu-stop
        crossfader-set-player
        crossfader-unset-player
        crossfader-set-visu-to-locked
        crossfader-set-visu-to-unlocked
        crossfader-save-controls
        crossfader-get-player-level
;        save-controls
    )
)

(define Crossfader%
    (class* object% (Crossfader-Interface)
        (init-field
            num
            owner
        )
        (field
            (side 0)
            (pause #f)
            (visu (new Visu% (id num)))
            (wait-default #f)
            (auto-mode #f)
            (swap-mode #f)
        )
        (define/public (get-control name id)
            (send visu get-control name)
        )
        (define/public (crossfader-set-player #:player player #:level (level #f))
(show-d "debug crossfader-set-player entry")
            (send visu set-visu-player #:player player)
        )
        (define/public (crossfader-unset-player #:player player)
            (send visu unset-visu-player #:player player)
        )
        (define/public (crossfader-visu-launch #:bank (bank #f) #:level (level 1) #:player player #:swap (swap #f))
(show-d "debug crossfader-visu-launch entry")
(show-d pause)
(show-d swap-mode)
            (cond
                (swap-mode
                    (cond
                        (pause
                            (send visu visu-launch)
                            (set! pause #f)
                        )
                        (else
                            (crossfader-visu-stop #:player player #:level level #:force #t)
                            (set! pause #t)
                        )
                    )
                )
                (else
(show-d "debug crossfader-visu-launch no swap mode")
                    (crossfader-visu-stop #:player player #:level level #:force #t)
(show-d "debug crossfader-visu-launch crossfader-visu-stop")
                    (send visu visu-launch)
(show-d "debug crossfader-visu-launch visu-launch")
                    (set! pause #f)
                )
            )
        )
        (define/public (crossfader-visu-stop #:player player #:bank (bank #f) #:level (level 1) #:force (force #f))
            (cond
                ((not (and auto-mode (not force) swap-mode))
                    (send visu visu-stop)
                    (set! pause #t)
                )
                
            )
        )
        (define/public (crossfader-set-visu #:visu visual #:mode (mode #f) #:velocity (velocity 1) #:player player #:auto (auto #f) #:swap (swap #f))
(show-d "debug crossfader-set-visu entry")
(show-d visual)
            (set! auto-mode auto)
            (set! swap-mode swap)
            (set! pause #t)
            (send visu set-visu #:visu visual #:mode mode #:velocity velocity #:player player)
        )
        (define/public (crossfader-set-visu-to-locked #:player player)
            (send visu set-visu-to-locked #:player player)
        )
        (define/public (crossfader-set-visu-to-unlocked #:player player)
            (send visu set-visu-to-unlocked #:player player)
        )
        (define/public (crossfader-save-controls #:player player)
            (send visu visu-save-controls #:player player)
        )
        (define/public (crossfader-get-player-level #:player player)
            #f
        )
        (define/public (crossfader-get-visu)
            (send visu get-visu)
        )
        (super-new)
    )
)

;Crossfader-Bank-public-interface
; get-control
; get-mode-auto
; crossfader-set-visu/
; crossfader-visu-launch*
; crossfader-visu-stop*
; crossfader-set-player*
; crossfader-unset-player*
; crossfader-set-visu-to-locked*
; crossfader-set-visu-to-unlocked*

;Crossfader-Bank-private-interface
; get-visu*
; add-visu*
; del-visu*
; get-level-from-id

(define Crossfader-Bank%
    (class* object% (Crossfader-Interface)
        (init-field
            num
            owner
        )
        (field
            (side 0)
            (pause #t)
            (visu #f)
            (visu-list (make-hash))
            (wait-default #f)
            (player-level (make-hash))
            (desired-mode 1)
            (last-mode 1)
            (auto-mode #f)
            (swap #f)
            
        )
        (define/private (get-visu level)
            (hash-ref visu-list level)
        )
        (define/private (get-level-from-id id)
;(show-d "debug get-level-from-id")
;(show-d id)
;(show-d (substring id 4 7))
            (string->number (substring id 4 7))
        )
        (define/public (get-control name id)
            (send (get-visu (get-level-from-id id)) get-control name)
        )
        (define/public (crossfader-set-player #:player player #:level (level #f))
(show-d "debug crossfader-set-player entry")
(show-d level)
            (when level
                (cond
                    (visu
    ;                    (crossfader-set-visu-to-locked #:player player #:level level)
                        (unless (hash-has-key? visu-list level)
                            (add-visu #:level level #:player player)
    ;                        (send visu-h set-visu #:visu visu #:mode -1 #:velocity velocity #:player player #:bank #t)
                            (crossfader-set-visu #:visu visu #:player player #:level level #:mode -1)
                        )
    ;                    (send (get-visu level) set-visu-to-unlocked #:player player)
                        (unless (hash-has-key? (send (get-visu level) get-players) player)
                            (send (get-visu level) set-visu-player #:player player)
                        )
                        (send (get-visu level) set-visu-to-control #:player player)
                        (hash-set! player-level player level)
(show-d "debug crossfader-set-player level")
(show-d player)
(show-d level)
(show-d (hash-ref player-level player))
                    )
                    (else
                        (hash-set! player-level player level)
                    )
                )
            )
        )
        (define/public (crossfader-unset-player #:player player)
(show-d "debug crossfader-unset-player entry")
            (send (hash-ref visu-list (hash-ref player-level player)) unset-visu-player #:player player)
;            (hash-set! player-level player #f)
        )
        (define/public (crossfader-set-visu-to-locked #:player player #:level (level #f))
            (hash-for-each
                visu-list
                (lambda (n_level visu-h)
                    (cond
                        (level
                            (when (equal? level n_level)
                                (send visu-h set-visu-to-locked #:player player)
                            )
                        )
                        (else
                            (send visu-h set-visu-to-locked #:player player)
                        )
                    )
                )
            )
        )
        (define/public (crossfader-set-visu-to-unlocked #:player player #:level (level #f))
            (hash-for-each
                visu-list
                (lambda (n_level visu-h)
                    (cond
                        (level
                            (when (equal? level n_level)
                                (send visu-h set-visu-to-unlocked #:player player)
                            )
                        )
                        (else
                            (send visu-h set-visu-to-unlocked #:player player)
                        )
                    )
                )
            )
        )
        (define/public (crossfader-set-visu #:visu visual #:level (level #f) #:mode (mode #f) #:velocity (velocity 1) #:player player #:auto (auto #f) #:swap (swap-v #f))
(show-d "crossfader-set-visu entry")
            (set! visu visual)
            (set! swap swap-v)
(show-d swap)
            (set! auto-mode auto)
            (cond
                (level
(show-d2 "crossfader-set-visu level parameter passed")
                    (send (hash-ref visu-list level) set-visu #:visu visu #:mode mode #:velocity velocity #:player player #:bank #t)
                )
                (else
                    (hash-for-each
                        visu-list
                        (lambda (n_level visu-h)
                            (cond
                                ((equal? (hash-ref player-level player #f) level)
(show-d2 "crossfader-set-visu player-level detected")
                                   (send visu-h set-visu #:visu visu #:mode n_level #:velocity velocity #:player player #:bank #t)
                                   (send visu-h set-visu #:visu visu #:mode -1 #:velocity velocity #:player player #:bank #t)
                                )
                                (else
(show-d2 "crossfader-set-visu no level")
(show-d2 n_level)
                                    (send visu-h set-visu #:visu visu #:mode n_level #:velocity velocity #:player player #:bank #t)
                                )
                            )
                        )
                    )
                )
            )
        )
        (define/public (add-visu #:level level #:player player)
(show-d "add-visu entry")
            (del-visu #:level level)
(show-d "add-visu del-visu")
            (hash-set!
                visu-list
                level
                (new
                    Visu%
                    (id
                        (string-append
                            (substring
                                (number->string
                                    (+ 1000 (string->number num))
                                )
                                1
                            )
                            "-"
                            (substring
                                (number->string
                                    (+ 1000 level)
                                )
                                1
                            )
                        )
                    )
                )
            )
(show-d "add-visu exit")
        )
        (define/private (del-visu #:level level)
            (when (hash-has-key? visu-list level)
                (send (get-visu level) visu-stop)
                (hash-remove! visu-list n)
            )
        )
        (define/public (crossfader-visu-launch #:bank (bank #t) #:level (level 1) #:velocity (velocity 1) #:player player #:swap (swap-v #f))
            (set! swap swap-v)
            (let
                (
                    (crossfader-launch-visu
                        (lambda ()
(show-d "debug crossfader-visu-launch entry")
                            (unless (hash-has-key? visu-list level)
(show-d "debug crossfader-visu-launch unless")
                                (add-visu #:level level #:player owner)
                                (when visu
                                    (crossfader-set-visu #:visu visu #:player player #:level level #:mode level)
                                )
(show-d "debug crossfader-visu-launch add-visu")
                            )
(show-d "debug crossfader-visu-launch unless passed")
(show-d visu)
                            (when (number? (hash-ref! player-level player #f))
                                (when (= level (hash-ref player-level player))
                                    (send (get-visu level) set-visu-to-control #:player player)
                                )
                            )
                            (when visu
(show-d "debug crossfader-visu-launch when")
                                (send (get-visu level) visu-launch)
(show-d "debug crossfader-visu-launch visu-launch passed")
                            )
                        )
                    )
                )
(show-d "swap : ")
(show-d swap)
                (cond
                    (swap
                        (cond
                            (pause
                                (crossfader-launch-visu)
                                (set! pause #f)
                            )
                            (else
                                (crossfader-visu-stop #:bank bank #:level level #:player player #:force #t)
                                (set! pause #t)
                            )
                        )
                    )
                    (else
                        (crossfader-launch-visu)
                    )
                )
            )
        )
        (define/public (crossfader-visu-stop #:bank (bank #t) #:level (level #f) #:player player #:force (force #f))
(show-d "debug crossfader-visu-stop entry")
(show-d player-level)
            (when level
                (cond
                    ((zero? level)
                        (hash-for-each
                            visu-list
                                (lambda (n_level visu-h)
                                    (send visu-h visu-stop)
                                        (when (= n_level (hash-ref player-level player #f))
                                            (send (get-visu n_level) set-visu-to-locked #:player player)
                                    )
                                )
                        )
                    )
                    (else    
                        (when (hash-has-key? visu-list level)
    (show-d "debug crossfader-visu-stop when has-key? visu-list-level")
                            (send (get-visu level) visu-stop)
    (show-d "debug crossfader-visu-stop visu-stop")
    (show-d "debug crossfader-visu-stop level :")
    (show-d level)
    (show-d "debug crossfader-visu-stop player-level :")
    (show-d player-level)
                            #(when (number? (hash-ref player-level player #f))
                                (when (= level (hash-ref player-level player #f))
    (show-d "debug crossfader-visu-stop when (= player level) set-visu-to_locked")
                                    (send (get-visu level) set-visu-to-locked #:player player)
                                )
                            )
                        )
                    )
                )
            )
        )
        (define/public (get-mode-auto)
            auto-mode
        )
        (define/public (crossfader-save-controls #:player player)
            (let
                (
                    (visu-target (get-visu (crossfader-get-player-level #:player player)))
                )
                (when visu-target
                    (send visu-target visu-save-controls #:player player #:level (crossfader-get-player-level #:player player))
                )
            )
        )
        (define/public (crossfader-get-player-level #:player player)
;(show-d "debug crossfader-get-player-level")
;(show-d player)
;(show-d (hash-ref player-level player #f))
            (hash-ref player-level player #f)
        )
        (define/public (crossfader-get-visu)
            visu
        )
        (super-new)
    )
)



(define (Control-Assign-Task Id Name File Players)
    (let
        (
;            (note-event (midi-note))
            (cc-event (midi-cc-event))
        )
(show "")
(show File)
(show Name)
        (letrec
            (
                (control-record
                    (lambda ()
                        (let*
                            (
                                (player
                                    (detect-player)
                                )
                                (output-file
                                    (cond
                                        (player
                                            (cond
                                                ((and (string? player) (string? File))
                                                    (string-append "controls/" File "." player)
                                                )
                                                (else
                                                    #f
                                                )
                                            )
                                        )
                                        (else
                                            #f
                                        )
                                    )
                                )
                            )
;(show-d "debug control-assign")
;(show-d Players)
;(show-d player)
                            (cond
                                (output-file
;(show-d "debug control-assign file")
                                    (cond
                                        ((and Control-Type-Temp Control-Address-Temp)
;(show-d "debug control-assign control-temp")
                                            (cond
                                                ((and (key-special-pressed 107) (not Control-Assign-Touch))
                                                    (set! Control-Assign-Touch #t)
                                                    (let ((out (open-output-file output-file #:mode 'binary #:exists 'append)))
                                                        (display File out)
                                                        (display #\newline out)
                                                        (display player out)
                                                        (display #\newline out)
                                                        (display Name out)
                                                        (display #\newline out)
                                                        (display Control-Type-Temp out)
                                                        (display #\newline out)
                                                        (display (string-append "(" "vector" " " (number->string (vector-ref Control-Address-Temp 0)) " " (number->string (vector-ref Control-Address-Temp 1)) ")") out)
                                                        (display #\newline out)
                                                        (display "0" out)
                                                        (display #\newline out)
                                                        (display "1" out)
                                                        (display #\newline out)
                                                        (display (Get-Control-Event) out)
                                                        (display #\newline out)
                                                        (display "1" out)
                                                        (display #\newline out)
                                                        (display "1" out)
                                                        (display #\newline out)
                                                        (display "1" out)
                                                        (display #\newline out)
                                                        (close-output-port out)
                                                        #f
                                                    )
                                                )
                                                ((and Control-Assign-Touch (not (key-special-pressed 107)))
                                                    (set! Control-Assign-Touch #f)
                                                    #f
                                                )
                                                (else
                                                    (Get-Control-Event)
                                                )
                                            )
                                        )
                                        (else
                                            (Get-Control-Event)
                                        )
                                    )
                                )
                                (else
                                    (Get-Control-Event)
                                )
                            )
                        )
                    )
                )
                (Get-Control-Event
                    (lambda ()
                        (cond
;                            ((equal? type "note")
;                                (vector-ref address-event 3)
;                            )
                            ((and (equal? Control-Type-Temp "midi-ccn") Control-Address-Temp)
                                (midi-ccn (vector-ref Control-Type-Temp 0) (vector-ref Control-Type-Temp 1))
                            )
                            (else
                                1
                            )
                        )
                    )
                )
                (detect-player
                    (lambda ()
;(show-n "debug debug")
;(show-n Players)
;(show-n (hash-iterate-key Players (hash-iterate-first Players)))
 ;                       (when (= 1 (hash-count Players))
 ;                           (hash-iterate-first Players)
 ;                       )
                        (when (hash-has-key? Players "Korg")
                            "Korg"
                        )
                    )
                )
            )
            (cond
                (cc-event
                    (set! Control-Type-Temp "ccn")
                    (set! Control-Address-Temp (vector (vector-ref cc-event 0) (vector-ref cc-event 1)))
                )
;                (note-event
;                    (control-record "note" note-event)
;                )
                (else
                    (unless Control-Assign-Temp
                        (set! Control-Assign-Temp 1)
                    )
                    Control-Assign-Temp
                )
            )
            (unless (control-record)
                (show-n "exit")
                #f
            )
        )
    )
)

(define Control-Type-Temp #f)
(define Control-Address-Temp #f)
(define Control-Assign-Temp 1)
(define Control-Assign-Touch #f)



;visu-public-interface
; get-control*
; save-controls/
; visu-launch*
; visu-stop*
; set-visu*
; set-visu-to-locked*
; set-visu-to-unlocked*
; set-visu-player*
; unset-visu-player*
; set-visu-to-control*
; get-visu

;visu-private-interface
; load-controls/
; get-visu-task-name*
; get-control-object/


(define Visu-Interface
    (interface ()
        get-control
        get-players
        get-visu
        visu-launch
        visu-stop
        set-visu
        set-visu-to-locked
        set-visu-to-unlocked
        set-visu-player
        unset-visu-player
        set-visu-to-control
        visu-save-controls
    )
)

(define Visu%
    (class* object% (Visu-Interface)
        (init-field
            id
        )
        (field
            (file #f)
            (controls (make-hash))
            (players (make-hash))
            (mode 1)
        )
        (define/private (control-assign name)
            (cond
                ((task-running? 'Control-Assign)
                    1
                )
                (else
                    (spawn-task (lambda () (Control-Assign-Task id name file players)) 'Control-Assign)
                    (cond
                        (Control-Assign-Temp
                            Control-Assign-Temp
                        )
                        (else
                            1
                        )  
                    )
;                   1
                )
            )
        )
        (define/public (get-players)
            players
        )
        (define/public (get-control name)
            (let
                ((control-object (get-control-object name)))
                (cond
                    (control-object
                        (send (get-control-object name) get-control)
                    )
                    (else
                        (control-assign name)
                    )
                )
            )
        )
        (define/private (get-control-object name)
            (hash-ref controls name #f)
        )
        (define/private (get-visu-task-name)
            (string->symbol
                (string-append
                    id
                    "-"
                    file
                )
            )
        )
        (define/private (load-player #:player player)
                (hash-set! players player #t)
                (load-controls player)
        )            
        (define/public (set-visu-player #:player player)
            (unless (hash-has-key? players player)
(show-n "debug set-visu-player")
(show-n player)
                (load-player #:player player)
            )
            (set-visu-to-unlocked #:player player)
            (set-visu-to-control #:player player)
        )
        (define/public (unset-visu-player #:player player)
            (when (> (hash-count players) 1)
                (hash-remove! players player)
;                (load-controls)
            )
            (set-visu-to-locked #:player player)
        )
        (define/public (set-visu #:visu visu #:mode n_mode #:velocity velocity #:player player #:bank (bank #f) #:level (level 1))
(show-d "set-visu-entry")
            (cond
                (file
                    (cond
                        ((task-running? (get-visu-task-name))
                            (visu-stop #:bank bank #:level level)
                            (set! file visu)
                            (set! mode n_mode)
                            (load-player #:player player)
                            (visu-launch)
                        )
                        (else
                            (set! file visu)
                            (set! mode n_mode)
                            (load-player #:player player)
                        )
                    )
                )
                (else
                    (set! file visu)
                    (set! mode n_mode)
                    (load-player #:player player)
                )
            )
        )
        (define/public (visu-launch)
(show-d "visu-launch entry")
(show-d file)
            (when file
                (unless (defined? file)
                    (show-n "Load file" file)
                    (load (string-append "visus/" file ".scm"))
                )
                (spawn-task (lambda () ((eval-string file) id 1)) (get-visu-task-name))
(show-d "debug visu-launch spawn-task")
;(show-d (ls-tasks))
            )
        )
        (define/public (visu-stop #:bank (bank #f) #:level (level 1))
(show-d "debug visu-stop entry")
            (when file
                (when (task-running? (get-visu-task-name))
(show-d "debug visu-stop when task running")
                    (rm-task (get-visu-task-name))
(show-d "debug visu-stop rm-task")
                    (
                        (eval-string
                            (string-append
                                file
                                "-"
                                "destroy"
                            )
                            (lambda ()
;                                (show
 ;                                   (string-append file "-destroy not found")
  ;                              )
                                (eval-string "void")
                            )
                        )
                        id
                    )
(show-d "debug visu-stop destroy")
                )
            )
        )
        (define/public (set-visu-to-locked #:player player)
            (hash-for-each
                controls
                (lambda (key objet)
                    (send objet set-control-to-locked #:player player)
                )
            )
        )
        (define/public (set-visu-to-unlocked #:player player)
            (hash-for-each
                controls
                (lambda (key objet)
                    (send objet set-control-to-unlocked #:player player)
                )
            )
        )
        (define/public (set-visu-to-control #:player player)
            (hash-for-each
                controls
                (lambda (key objet)
                    (send objet set-control-to-control #:player player)
                )
            )
        )
        (define/private (load-controls (player-target #f))
            (let
                (
                    (load-player-controls
                        (lambda (player)
                            (let*
                                (
                                    (control-file
                                        (cond
                                            (file
                                                (cond
                                                    ((file-exists?  (string-append "controls/" file "." player))
                                                        (show-n (string-append "Personnalized Player File : " "controls/" file "." player))
                                                        (string-append "controls/" file "." player)
                                                    )
                                                    ((file-exists?  (string-append "controls/" file))
                                                        (string-append "controls/" file)
                                                    )
                                                    (else #f)
                                                )
                                            )
                                            (else #f)
                                        )
                                    )
                                    (in
                                        (if control-file
                                            (open-input-file control-file #:mode 'text)
                                            #f
                                        )
                                    )
                                )
                                (letrec
                                    (
                                        (Control-File-Parse
                                            (lambda (Id Player)
                                                (let*
                                                    (
                                                        (FILE      (read-line in))         ;File
                                                        (PLAYER    (read-line in))         ;Player
                                                        (NAME      (read-line in))         ;Name
                                                        (CONTROL   (read-line in))         ;Control Type
                                                        (ADDRESS   (read-line in))         ;Param value
                                                        (MAPPING   (read-line in))         ;Option (Mapping)
                                                        (ACTMODE   (read-line in))         ;ActMode
                                                        (DEFAULT1  (read-line in))         ;Default Value 1
                                                        (DEFAULT2  (read-line in))         ;Default Value 2
                                                        (DEFAULT3  (read-line in))         ;Default Value 3
                                                        (DEFAULT4  (read-line in))         ;Default Value 4
                                                    )
        (show-d "debug load-controls controls params read")
        (show-d control-file)
        (show-d FILE)
        (show-d NAME)
        (show-d PLAYER)
        (show-d Player)
                                                    (when
                                                        (not
                                                            (or
                                                                (eof-object? FILE)
                                                                (eof-object? NAME)
                                                                (eof-object? PLAYER)
                                                                (eof-object? CONTROL)
                                                                (eof-object? ADDRESS)
                                                                (eof-object? MAPPING)
                                                                (eof-object? ACTMODE)
                                                                (eof-object? DEFAULT1)
                                                                (eof-object? DEFAULT2)
                                                                (eof-object? DEFAULT3)
                                                                (eof-object? DEFAULT4)
                                                            )
                                                        )
        (show-d "debug load-controls valid-control")
                                                        (when (and (equal? FILE file) (equal? Player PLAYER))
        (show-d "debug load-controls valid-control with player")
                                                            (unless (hash-has-key? controls NAME)
        (show-d "debug load-controls add control to controls")
                                                                (hash-set! controls NAME (new Controls%))
                                                            )
                                                                (send (get-control-object NAME) set-control-type CONTROL #:player PLAYER)
                                                                (send (get-control-object NAME) set-control-address (eval-string ADDRESS) #:player PLAYER)
                                                                (send (get-control-object NAME) set-control-mapping (eval-string MAPPING) #:player PLAYER)
                                                                (send (get-control-object NAME) set-control-default 1 (eval-string DEFAULT1) #:player PLAYER)
                                                                (send (get-control-object NAME) set-control-default 2 (eval-string DEFAULT2) #:player PLAYER)
                                                                (send (get-control-object NAME) set-control-default 3 (eval-string DEFAULT3) #:player PLAYER)
                                                                (send (get-control-object NAME) set-control-default 4 (eval-string DEFAULT4) #:player PLAYER)
                                                                (send (get-control-object NAME) set-control-mode mode #:player PLAYER)
        ;                                                        (send (get-control-object NAME) set-control-actmode (eval-string ACTMODE) #:player PLAYER)
                                                        )
                                                        (Control-File-Parse Id Player)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                    (when in
                                        (file-position in 0)
                                        (Control-File-Parse id player)
                                        (file-position in 0)
                                        (close-input-port in)
        (show-d "debug load-control load end")
        (show-d controls)
                                    )
                                )
                            )
                        )
                    )
                )
    (show-d2 "debug load-controls entry")
                (cond
                    (player-target
                        (load-player-controls player-target)
                    )
                    (else
                        (hash-for-each
                            players
                            (lambda (player value)
                                (load-player-controls player)
                            )
                        )
                    )
                )
            )
        )
        (define/private (visu-search-control #:player player name default default-value control-actual level-to-keep)
            (let*
                (
                    (control-file
                        (cond
                            (file
                                (cond
                                    ((file-exists?  (string-append "controls/" file "." player))
                                        (show-n (string-append "Personnalized Player File : " "controls/" file "." player))
                                        (string-append "controls/" file "." player)
                                    )
                                    ((file-exists?  (string-append "controls/" file))
                                        (string-append "controls/" file)
                                    )
                                    (else #f)
                                )
                            )
                            (else #f)
                        )
                    )
                    (in
                        (if control-file
                            (open-input-file control-file #:mode 'text)
                            #f
                        )
                    )
                )
                (letrec
                    (
                        (Control-Search-In-File
                            (lambda (n)
                                (let
                                    (
                                        (line (read-line in))
                                    )
                                    (cond
                                        ((equal? line n)
                                            #t
                                        )
                                        ((eof-object? line)
                                            #f
                                        )
                                        (else (Control-Search-In-File n))
                                    )
                                )
                            )
                        )
                        (Control-Value-Search-In-File
                            (lambda (v)
                                (let*
                                    (
                                        (value0 4)
                                        (value (+ value0 v))
                                    )
                                    (Control-Value-Search-In-File-Sub value)
                                )
                            )
                        )
                        (Control-Value-Search-In-File-Sub
                            (lambda (value)
                                (let
                                    (
                                        (line (read-line in))
                                    )
                                    (cond
                                        ((= value 1)
                                            line
                                        )
                                        ((eof-object? line)
                                                #f
                                            )
                                        (else (Control-Value-Search-In-File-Sub (- value 1)))
                                    )
                                )
                            )
                        )
                    )
                    (when in
                        (file-position in 0)
                        (Control-Search-In-File name)
                        (let
                            (
                                (value-result (Control-Value-Search-In-File default))
                            )
                            (file-position in 0)
                            (close-input-port in)
                            (cond
                                ((= default level-to-keep)
;(show "debug level-to-keep")
;(show level-to-keep)
                                    control-actual
                                )
                                (value-result
                                    value-result
                                )
                                (else
                                    default-value
                                )
                            )
                        )
                    )
                )
            )
        )
;        (define/private (visu-search-control #:player player name default default-value)
        (define/public (visu-save-controls #:player player #:level (level-player 1))
;(show "debug visu-save-control")
;(show level-player)
            (let ((controls-default-temp (make-hash)))
                (hash-for-each
                    controls
                    (lambda (Name C)
                        (let*
                            (
;                                (get-control-actual (send C get-control))
                                (get-control-actual (get-control Name))
                                (get-default-value-lambda
                                    (lambda (n)
                                        (let
                                            (
                                                (value-default (visu-search-control #:player player Name n (send C get-control-default n #:player player) get-control-actual level-player))
                                            )
                                            (cond
                                                ((number? value-default)
                                                    (real->double-flonum value-default)
                                                )
                                                ((string? value-default)
                                                    (real->double-flonum (string->number value-default))
                                                )
                                            )
                                        )
                                    )
                                )
                                (default-value-1
                                    (get-default-value-lambda 1)
                                )
                                (default-value-2
                                    (get-default-value-lambda 1)
                                )
                                (default-value-3
                                    (get-default-value-lambda 1)
                                )
                                (default-value-4
                                    (get-default-value-lambda 1)
                                )
                            )
                            (hash-set! controls-default-temp Name (list default-value-1 default-value-2 default-value-3 default-value-4))
                        )
                    )
                )
;(show controls-default-temp)

                (let*
                    (
                        (output-file (string-append "controls/" file "." player))
                        (out (open-output-file output-file #:mode 'binary #:exists 'truncate))
                    )
                    (hash-for-each controls
                        (lambda (Name C)
;(show-n "")
;(show-n output-file)
;(show-n Name)
;(show-n C)
;(show (list-ref (hash-ref controls-default-temp Name) 0))
;(show (list-ref (hash-ref controls-default-temp Name) 1))
;(show (list-ref (hash-ref controls-default-temp Name) 2))
;(show (list-ref (hash-ref controls-default-temp Name) 3))
                                (display file out)
                                (display #\newline out)
                                (display player out)
                                (display #\newline out)
                                (display Name out)
                                (display #\newline out)
                                (display (send C get-control-type #:player player) out)
                                (display #\newline out)
                                (display (send C get-control-address #:player player) out)
;(display "adress : ")(show-n (send C get-control-address))
                               (display #\newline out)
                               (display (send C get-control-mapping #:player player) out)
                               (display #\newline out)
                               (display (send C get-control-mode #:player player) out)
                               (display #\newline out)
                               (display (list-ref (hash-ref controls-default-temp Name) 0) out)
                               (display #\newline out)
                               (display (list-ref (hash-ref controls-default-temp Name) 1) out)
                               (display #\newline out)
                               (display (list-ref (hash-ref controls-default-temp Name) 2) out)
                               (display #\newline out)
                               (display (list-ref (hash-ref controls-default-temp Name) 3) out)
                               (display #\newline out)
                        )
                    )
                    (close-output-port out)
                )
            )
        )
        (define/public (get-visu)
            file
        )
        (super-new)
    )
)

;Controls-public-interface
; get-control/
; set-control-type control #:player*
; set-control-address address #:player*
; set-control-mapping mapping #:player*
; set-control-actmode actmode #:player*
; set-control-default default value #:player*
; set-control-mode mode #:player*
; get-control-type #:player*
; get-control-address #:player*
; get-control-mapping #:player*
; get-control-mode #:player*
; get-control-default default #:player*
; set-control-to-locked #:player*
; set-control-to-unlocked #:player*
; set-control-to-control #:player*

;Controls-private-interface
; get-player*
; check-hand/
; check-player*
; create-control/

(define Controls-Interface
    (interface ()
        set-control-type
        set-control-address
        set-control-mapping
        set-control-actmode
        set-control-default
        set-control-mode
        set-control-to-locked
        set-control-to-unlocked
        set-control-to-control
        get-control-type
        get-control-address
        get-control-mapping
        get-control-mode
        get-control-default
    )
)

(define Controls%
    (class* object% (Controls-Interface)
        (field
            (players (make-hash))
            (hand #f)
            (mode 1)
            (locked-value 1)
            (check-hand-ecart (make-hash))
            (last-check-hand #f)
        )
        (define/private (create-control control)
            (cond
                ((equal? control "ccn")
(show-d "debug create-control ccn")
(show-d control)
                    (new Control-Midi-CCN%)
                )
                ((equal? control "midi-ccn")
                    (new Control-Midi-CCN%)
                )
                ((equal? control "kb")
                    (new Control-Keyboard%)
                )
                ((equal? control "kbs")
(show-d "debug create-control kbs")
                    (new Control-Keyboard-Special%)
                )
                ((equal? control "osc")
                    (new Control-Osc%)
                )
                ((equal? control "fake")
                    (new Control-Fake%)
                )
            )
        )
        (define/private (check-player player)
            (hash-has-key? players player)
        )
        (define/private (get-player (player hand))
            (hash-ref players player)
        )
        (define/public (get-control)
;(show-d "debug Controls-get-control entry")
;(show-d players)
            (let ((value (send (get-player) get-control)))
                (check-hand value)
                value
            )
        )
        (define/private (check-hand current-value)
            (unless (= 1 (hash-count players))
;(show "nb-player passed")
                (hash-for-each
                    players
                    (lambda (player control)
                        (unless (equal? player hand)
;(show "other player passed")
                            (when (= -1 (get-control-mode #:player player))
;(show "control mode passed")
;(show player)
                                (let ((current-player-value (send control get-control)))
;(show current-player-value)
                                    (cond
                                        (last-check-hand
                                            (cond
                                                ((< (check-ecart current-value current-player-value) 0.05)
;(show "equal values passed")
                                                    (when
                                                        (or
                                                            (> (check-ecart current-value last-check-hand) 0.1)
                                                            (check-hand-ecart-passed? player)
                                                        )
;(show "check-hand-passed")
                                                        (set! last-check-hand current-value)
                                                        (hash-set! check-hand-ecart player #f)
                                                        (set! hand player)
                                                    )
                                                )
                                                (else
                                                    (when (> (check-ecart current-player-value last-check-hand) 0.1)
;(show "check-ecart passed")
                                                        (hash-set! check-hand-ecart player #t)
                                                    )
                                                )
                                            )
                                        )
                                        (else
                                            (when (= current-value current-player-value)
                                                    (set! last-check-hand current-value)
                                                    (hash-set! check-hand-ecart player #f)
                                                    (set! hand player)
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        (define/private (check-ecart reference variable)
            (abs (- variable reference))
        )
;todo:
;define check-ecart
;check-hand-ecart (make-hash)
;last-check-hand #f
        (define/private (check-hand-ecart-passed? player)
            (hash-ref check-hand-ecart player #f)
        )
        (define/public (set-control-type control #:player player)
            (hash-set! players player (create-control control))
            (unless hand
                (set! hand player)
            )
        )
        (define/public (set-control-address address #:player player)
            (when (check-player player)
(show-d "debug set-address")
(show-d players)
(show-d player)
(show-d (get-player player))
                (send (get-player player) set-address address)
            )
        )
        (define/public (set-control-mapping mapping #:player player)
            (when (check-player player)
                (send (get-player player) set-mapping mapping)
            )
        )
        (define/public (set-control-actmode actmode #:player player)
            (when (check-player player)
                (send (get-player player) set-actmode actmode)
            )
        )
        (define/public (set-control-default default value #:player player)
            (when (check-player player)
                (send (get-player player) set-default-value default value)
            )
        )
        (define/public (set-control-mode Mode #:player player)
            (when (check-player player)
                (send (get-player player) set-mode Mode)
            )
        )
        (define/public (set-control-to-locked #:player player)
            (when (check-player player)
                (send (get-player player) set-to-locked)
            )
        )
        (define/public (set-control-to-unlocked #:player player)
            (when (check-player player)
                (send (get-player player) set-to-unlocked)
            )
        )
        (define/public (set-control-to-control #:player player)
            (when (check-player player)
                (send (get-player player) set-to-control)
            )
        )
        (define/public (get-control-type #:player player)
            (when (check-player player)
                (send (get-player player) get-type)
            )
        )
        (define/public (get-control-address #:player player)
            (when (check-player player)
                (send (get-player player) get-address)
            )
        )
        (define/public (get-control-mapping #:player player)
            (when (check-player player)
                (send (get-player player) get-mapping)
            )
        )
        (define/public (get-control-mode #:player player)
            (when (check-player player)
                (send (get-player player) get-mode)
            )
        )
        (define/public (get-control-default default #:player player)
            (when (check-player player)
                (send (get-player player) get-default default)
            )
        )
        (super-new)
    )
)

;Control-public-interface
; set-address address*
; set-mapping mapping*
; set-actmode*
; set-default-value value*
; set-mode mode*
; set-to-locked*
; set-to-unlocked*
; set-to-control*
; get-type*
; get-address*
; get-mapping*
; get-mode*
; get-default default*
; get-control*
; get-control-mode*

;Control-private-interface
; get-locked-mode
; get-control-locked-mode

(define Control-Interface
    (interface ()
        set-address
        set-mapping
        set-actmode
        set-default-value
        set-mode
        set-to-locked
        set-to-unlocked
        set-to-control
        set-to-default
        get-type
        get-address
        get-mapping
        get-mode
        get-default
        get-control
        get-control-mode
    )
)

(define Control%
    (class* object% (Control-Interface)
        (field
            (address (vector 0 0))
            (mapping #f)
            (default-values (make-hash (list (cons 0 0.5) (cons 1 1) (cons 2 1) (cons 3 1) (cons 4 1))))
            (mode 1)
            (mode-previous-lock #f)
            (locked-value #f)
        )
        (define/public (set-address n_address)
            (set! address n_address)
        )
        (define/public (set-mapping n_mapping)
            (set! mapping n_mapping)
        )
        (define/public (set-actmode n_mode)
(show-d2 "debug set-act-mode")
            (set-mode n_mode)
        )
        (define/public (set-default-value default value)
            (hash-set! default-values default value)
        )
        (define/public (set-mode n_mode)
(show-d2 address)
            (case n_mode
                [(-1)
(show-d2 "debug set-mode set-to-control")
                    (set-to-control)
                ]
                [(0)
                    (set-to-locked)
(show-d2 "debug set-mode set-to-locked")
                ]
                [(1 2 3 4)
                    (set-to-default n_mode)
(show-d2 "debug set-mode set-to-default")
(show-d2 n_mode)
                ]
            )
        )
        (define/public (set-to-locked)
(show-d2 "debug set-to-locked entry")
(show-d2 address)
(show-d2 (get-control))
            (set! locked-value (get-control))
(show-d2 locked-value)
            (set! mode-previous-lock mode)
            (set! mode 0)
        )
        (define/public (set-to-unlocked)
(show-d2 "debug set-to-unlocked entry")
            (when mode-previous-lock
                (set! mode mode-previous-lock)
            )
        )
        (define/public (set-to-control)
(show-d2 "debug set-to-control entry")
            (cond
                ((equal? mode 0)
(show-d2 "debug set-to-control mode = 0")
(show-d2 address)
(show-d2 locked-value)
                    (set-to-unlocked)
                    (set! mode -1)
                )
                (else
                    (cond
                        (locked-value
(show-d2 "debug set-to-control locked-value #t")
                            (set! mode -1)
                        )
                        (else
(show-d2 "debug set-to-control locked-value #f")
                            (set! locked-value (get-control))
                            (set! mode -1)
                        )
                    )
                )
            )
        )
        (define/public (set-to-default n_mode)
(show-d "debug set-to-default entry")
            (set! mode n_mode)
        )
        (define/public (get-type)
            "ctrl"
        )
        (define/public (get-address)
            address
        )
        (define/public (get-mapping)
            mapping
        )
        (define/public (get-mode)
            mode
        )
        (define/public (get-default default)
            (hash-ref default-values default #f)
        )
        (define/public (get-control)
            (case mode
                [(-1)
                    (if locked-value
                        (get-control-locked-mode)
                        (get-control-mode)
                    )
                ]
                [(0)
                    (get-locked-mode)
                ]
                [(1 2 3 4)
                    (get-default mode)
                ]
            )
        )
        (define/public (get-control-mode)
            1
        )
        (define/private (get-control-locked-mode)
(show-d2 "debug get-control-locked-mode entry")
            (let
                (
                    (offset 0.1)
                    (control-mode (get-control-mode))
                    (locked-mode (get-locked-mode))
                )
(show-d2 "debug get-control-locked-mode let")
(show-d2 "control-mode :")
(show-d2 control-mode)
(show-d2 "locked-mode :")
(show-d2 locked-mode)
(show-d2 "locked-value :")
(show-d2 locked-value)
(show-d2 "")
                (cond
                    ((not (equal? (void) locked-mode))
    (show-d2 "BBBBBBBBBBBBBBBBBBBUUUUUUUUUUUUUUUUUUUUUUUGGGGGGGGGGGGGGGGGGGGGGG !")
    (show-d2 locked-mode)
                        (cond
                            (
                                (and
                                    (> control-mode (- locked-mode offset))
                                    (< control-mode (+ locked-mode offset))
                                )
(show-d2 "debug get-control-locked-mode set! locked-value #f")
                                (set! locked-value #f)
                                control-mode
                            )
                            (else
                                locked-mode
                            )
                        )
                    )
                    (else
                        (cond
                            ((number? (get-control))
(show-d2 "debug get-control-locked-mode get-control")
                                (set! locked-value (get-control))
                                (get-control)
                            )
                            (else
                                (get-default 1)
                            )
                        )
                    )
                )
            )
        )
        (define/private (get-locked-mode)
(show-d2 "debug get-locked-mode")
            locked-value
        )
        (super-new)
    )
)

(define Control-Midi-CCN%
    (class* Control% (Control-Interface)
        (inherit-field
            address
        )
        (define/override (get-control-mode)
            (mn (vector-ref address 0) (vector-ref address 1))
        )
        (define/override (get-type)
            "midi-ccn"
        )
        (super-new)
    )
)

(define Control-Fake%
    (class* Control% (Control-Interface)
        (field
            (default-mode 1)
        )
        (inherit-field
            mode
        )
        (inherit
            get-default
        )
        (define/override (set-to-default n_mode)
            (set! mode n_mode)
            (set! default-mode n_mode)
        )
        (define/override (get-control-mode)
            (get-default default-mode)
        )
        (define/override (get-type)
            "fake"
        )
        (super-new)
    )
)

(define Control-Keyboard-Special%
    (class* Control% (Control-Interface)
        (inherit-field
            address
        )
        (define/override (get-control-mode)
            (key-special-pressed address)
        )
        (define/override (get-type)
            "kbs"
        )
        (super-new)
    )
)









(define Triggers%
    (class object%
        (field
            (trigger-list (make-hash))
        )
        (define/public (add-trigger n t)
            (hash-set! trigger-list n t)
        )
        (define/public (midi-note-read)
            (let
                (
                    (midi-note-buffer (midi-note))
                )
                (when midi-note-buffer
                    (let
                        (
                            (trigger-name (string-append "note" "-" (substring (number->string (+ 100 (vector-ref midi-note-buffer 1))) 1) "-" (substring (number->string (+ 1000 (vector-ref midi-note-buffer 2))) 1)))
                        )
                        (when (hash-has-key? trigger-list trigger-name)
                            (if (eq? (vector-ref midi-note-buffer 0) 'note-off)
                                (send (hash-ref trigger-list trigger-name) set-value 0)
                                (send (hash-ref trigger-list trigger-name) set-value (vector-ref midi-note-buffer 3))
                            )
                        )
                    )
                )
            )
        )
        (define/public (test-triggers)
;(show-d "test-triggers")
            (midi-note-read)
;(show-d "note parsed")
            (hash-for-each
                trigger-list
                (lambda (n t)
;(show-d n)
                    (let
                        (
                            (trigg-activ? (send t trigg?))
                        )
;(show-d "trigg-activ?")
;(show-d trigg-activ?)
;(show-d (string? trigg-activ?))
;(show-d "")
                        (when trigg-activ?
                            (unless (equal? trigg-activ? "(void)")
                                (show-n (string-append "Launch " trigg-activ?))
                            )
                            (eval-string trigg-activ?)
                        )
                    )
                )
            )
        )
        (define/public (load-triggers (file TRIGGER-SAVE-FILE))
            (let ((in (open-input-file file #:mode 'text)))
                (letrec
                    (
                        (Trigger-File-Parse
                            (lambda ()
                                (let
                                    (
                                        (s (read-line in))  ;Nom
                                        (p (read-line in))  ;Player
                                        (t (read-line in))  ;Type
                                        (u (read-line in))  ;Adress
                                        (v (read-line in))  ;Fonction On
                                        (v2 (read-line in)) ;Fonction Off
                                        (w (read-line in))  ;Mapping?
                                    )
                                    (when (not (or (eof-object? s) (eof-object? p) (eof-object? t) (eof-object? u) (eof-object? v) (eof-object? v2) (eof-object? w)))
                                        (let
                                            (
                                                (new-trigger
                                                    (cond
                                                        ((equal? t "note")
                                                            (new Trigger-Midi-Note%))
                                                        ((equal? t "kb")
                                                            (new Trigger-Keyboard%))
                                                        ((equal? t "kbs")
                                                            (new Trigger-Keyboard-Special%))
                                                        ((equal? t "midi-ccn")
                                                            (new Trigger-Midi-CCN%))
                                                        ((equal? t "ctrl")
                                                            (new Trigger-Ctrl%))
                                                    )
                                                )
                                            )
                                            (send new-trigger set-control u)
                                            (send new-trigger set-function-on v)
                                            (send new-trigger set-function-off v2)
                                            (send new-trigger set-player p)
                                            (let
                                                (
                                                    (new-trigger-name
                                                        (cond
                                                            ((equal? t "note")
                                                                (string-append "note" "-" (substring (number->string (+ 100 (send new-trigger get-channel))) 1) "-" (substring (number->string (+ 1000 (send new-trigger get-note))) 1))
                                                            )
                                                            ((equal? t "kb")
                                                                (string-append "kb" "-" (send new-trigger get-key))
                                                            )
                                                            ((equal? t "kbs")
                                                                (string-append "kbs" "-" (number->string (send new-trigger get-key))
                                                                )
                                                            )
                                                            ((equal? t "midi-ccn")
                                                                (string-append "midi-ccn" "-" (substring (number->string (+ 100 (send new-trigger get-channel))) 1) "-" (substring (number->string (+ 1000 (send new-trigger get-button))) 1))
                                                            )
                                                        )
                                                    )
                                                )
                                            (add-trigger new-trigger-name new-trigger)
(show-n (string-append s " loaded with name " new-trigger-name))
                                            )
                                        )
                                        (Trigger-File-Parse)
                                    )
                                )
                            )
                        )
                    )
                (file-position in 0)
                (Trigger-File-Parse)
                (close-input-port in)
                )
            )
        )
        (super-new)
    )
)

(define Keyboard%
    (class object%
        (field
            (key #f)
        )
        (define/public (set-key n)
            (if (string? n)
                (set! key n)
                (set! key (number->string n))
            )
        )
        (define/public (get-key)
            key
        )
        (define/public (get-value)
            (key-pressed key)
        )
        (super-new)
    )
)

(define Keyboard-Special%
    (class Keyboard%
        (inherit-field
            key
        )
        (define/override (set-key n)
            (cond
                ((string? n)
                    (set! key (string->number n))
                )
                (else
                    (set! key n)
                )
            )
        )
        (define/override (get-value)
            (key-special-pressed key)
        )
        (super-new)
    )
)

(define Midi-CCN%
    (class object%
        (field
            (channel 0)
            (button 0)
        )
        (define/public (set-channel n)
            (set! channel n)
        )
        (define/public (set-button n)
            (set! button n)
        )
        (define/public (get-value)
            (mn channel button)
        )
        (define/public (get-channel)
            channel
        )
        (define/public (get-button)
            button
        )
        (super-new)
    )
)

(define Midi-Note%
    (class object%
        (field
            (channel 0)
            (note 0)
        )
        (define/public (set-channel n)
            (set! channel n)
        )
        (define/public (set-note n)
            (set! note n)
        )
        (define/public (get-channel)
            channel
        )
        (define/public (get-note)
            note
        )
        (super-new)
    )
)


(define Trigger-Midi-Note%
    (class Midi-Note%
        (field
            (function-on (void))
            (function-off (void))
            (triggered #f)
            (player #f)
            (value 0)
        )
        (define/public (set-control n)
            (let
                (
                    (v
                        (cond
                            ((vector? n)
                                n
                            )
                            ((string? n)
                                (eval-string n)
                            )
                        )
                    )
                )
                (when (vector? v)
                    (when (= (vector-length v) 2)
                        (send this set-channel (vector-ref v 0))
                        (send this set-note (vector-ref v 1))
;(show-n "note set control debug")
;(show-n (send this get-channel))
;(show-n (send this get-note))
                    )
                )
            )
        )
        (define/public (set-value n)
            (unless (= n value)
                (set! triggered #t)
                (set! value n)
            )
        )
        (define/public (set-function-on n)
            (when (string? n)
                (set! function-on n)
            )
        )
        (define/public (set-function-off n)
            (when (string? n)
                (set! function-off n)
            )
        )
        (define/public (set-player n)
            (set! player n)
        )
        (define/public (trigg?)
            (cond
                (triggered
                    (cond
                        ((zero? value)
                            (set! triggered #f)
                            function-off
                        )
                        (else
;                            (show-n (string-append "Launch " function-on))
                            (set! triggered #f)
                            function-on
                        )
                    )
                )
                (else
                    #f
                )
            )
        )
        (define/public (get-function-on)
            function-on
        )
        (define/public (get-function-off)
            function-off
        )
        (super-new)
    )
)

(define Trigger-Midi-CCN%
    (class Midi-CCN%
        (field
            (function-on (void))
            (function-off (void))
            (triggered #f)
            (player #f)
            (value 0)
        )
        (define/public (set-control n)
            (let
                (
                    (v
                        (cond
                            ((vector? n)
                                n
                            )
                            ((string? n)
                                (eval-string n)
                            )
                        )
                    )
                )
                (when (vector? v)
                    (when (= (vector-length v) 2)
                        (send this set-channel (vector-ref v 0))
                        (send this set-button (vector-ref v 1))
                    )
                )
            )
        )
        (define/public (set-value n)
            (unless (= n value)
                (set! triggered #t)
                (set! value n)
            )
        )
        (define/public (set-function-on n)
            (when (string? n)
                (set! function-on n)
            )
        )
        (define/public (set-function-off n)
            (when (string? n)
                (set! function-off n)
            )
        )
        (define/public (set-player n)
            (set! player n)
        )
        (define/public (trigg?)
            (cond
                ((and (= 1 (send this get-value)) (not triggered))
                    (set! triggered #t)
                    function-on
                )
                ((and (= (send this get-value) 0) triggered)
                    (set! triggered #f)
                    function-off
                )
                (else
                    #f
                )
            )
        )
        (define/public (get-function-on)
            function-on
        )
        (define/public (get-function-off)
            function-off
        )
        (super-new)
    )
)


(define Trigger-Keyboard%
    (class Keyboard%
        (inherit-field
            key
        )
        (field
            (function-on (void))
            (function-off (void))
            (triggered #f)
            (on #f)
            (option #f)
            (value 0)
            (Player #f)
        )
        (define/public (set-function-on n)
            (when (string? n)
                (set! function-on n)
            )
        )
        (define/public (set-function-off n)
            (when (string? n)
                (set! function-off n)
            )
        )
        (define/public (set-player player)
            (set! Player player)
        )
        (define/public (set-option n)
            (set! option n)
        )
        (define/public (set-control n)
            (cond
                ((string? n)
                    (send this set-key n)
                )
                ((number? n)
                    (send this set-key (number->string n))
                )
            )
        )
        (define/public (trigg?)
            (cond
                ((and (send this get-value) (not triggered))
                    (set! triggered #t)
                    function-on
                )
                ((and (not (send this get-value)) triggered)
                    (set! triggered #f)
                    function-off
                )
                (else
                    #f
                )
            )
        )
        (super-new)
    )
)

(define Trigger-Keyboard-Special%
    (class Keyboard-Special%
        (inherit-field
            key
        )
        (field
            (function-on (void))
            (function-off (void))
            (triggered #f)
            (on #f)
            (option #f)
            (value 0)
            (Player #f)
        )
        (define/public (set-function-on n)
            (when (string? n)
                (set! function-on n)
            )
        )
        (define/public (set-function-off n)
            (when (string? n)
                (set! function-off n)
            )
        )
        (define/public (set-option n)
            (set! option n)
        )
        (define/public (set-control n)
            (send this set-key n)
        )
        (define/public (set-player player)
            (set! Player player)
        )
        (define/public (trigg?)
;(show-d "debug trigger-key-special")
;(show-d (send this get-value))
            (cond
                ((and (send this get-value) (not triggered))
                    (set! triggered #t)
                    function-on
                )
                ((and (not (send this get-value)) triggered)
                    (set! triggered #f)
                    function-off
                )
                (else
                    #f
                )
            )
        )
        (super-new)
    )
)
            
(define god (new Crossfaders%))
(define (c name id #:type (type 'linear) #:coeff (coefficient 1) #:toggle (toggle #f))
    (* (send god get-control name id) coefficient)
)


(define Triggers-List (new Triggers%))
;(send Triggers-List load-triggers)
(load "load-config")
(spawn-task (lambda () (send Triggers-List test-triggers)) 'Trigger-Detect)

(define (gain-task)
    (let ((var-mouse-wheel (mouse-wheel)))
        (unless (zero? var-mouse-wheel)
            (cond
                ((= var-mouse-wheel 1)
                    (set-gain! (+ (get-gain) 0.1))
                )
                ((= var-mouse-wheel -1)
                    (set-gain! (- (get-gain) 0.1)))
                )
        )
    )
)

(spawn-task (lambda () (gain-task)) 'gain-task)