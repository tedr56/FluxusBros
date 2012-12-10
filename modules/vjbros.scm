(module vjbros racket
    (require fluxus-018/fluxus-midi)
    (require fluxus-018/fluxus)
    (require fluxus-018/time)
    (require scheme/math)
    (require scheme/string)
    (provide
        midi-connect
        m
        mn
        show
        gl
        reverse-string
        vector-of
        gh2
        opa-cross
        get-gain
        set-gain!
        tempo
        travel
        travel-end
        defined?
        neg
        degre->pi
        cylindric->cartesien
        cylindric-rad->cartesien
        cartesien->cylindric
        cartesien->cylindric-deg
        find
    	vector-non-empty?
    	defil
	    defil-x
        defil-y
        defil-z
        modulo-d
    )
    (define (midi-connect)
        (letrec
            (
                (midi-search
                    (lambda (d l (n 0))
                        (cond
                            ((equal? (symbol->string (cdr (list-ref l n))) d)
                                (car (list-ref l n))
                            )
                            (else
                                (if (< n (- (length l) 1))
                                    (midi-search d l (+ 1 n))
                                    #f
                                )
                            )
                        )
                    )
                )
                (midi-get-io
                    (lambda (io)
                        (if (equal? io "in")
                            (car (midi-info))
                            (car (cdr (midi-info)))
                        )
                    )
                )
            )
            (let
                (
                    (bit-in (midi-search "Bitstream 3X:0" (midi-get-io "in")))
                    (vmxvj-in (midi-search "VMXVJ" (midi-get-io "in")))
                    (Korg-in (midi-search "microKONTROL:1" (midi-get-io "in")))
                    (bit-out (midi-search "Bitstream 3X:0" (midi-get-io "out")))
                )
                (cond
                    ((and bit-in vmxvj-in)
                        (cond
                            ((midi-search "Midi Through:0" (midi-get-io "in"))
                                (midiin-open (midi-search "Midi Through:0" (midi-get-io "in")))
                            )
                            (else
                                (midiin-open bit-in)
                            )
                        )
                    )
                    (bit-in
(show "Connected to Bitstream 3X input port")
                        (midiin-open bit-in)
                    )
                    (Korg-in
                        (midiin-open Korg-in)
(show "Connected to Korg microKONTROL input port")
                    )
                    (else
                        (midiin-open 0)
                    )
                )
                (when bit-out
(show "Connected to Bitstream 3X output port")
                    (midiout-open bit-out)
                )
            )
        )
    )
    (define m midi-cc)
    (define (mn channel input (coeff 1) (name 0))
        (* (midi-ccn channel input) coeff))

    (define (show input . other)
;(display "debug show1")
;(newline)
;(display other)
;(newline)
;(display input)
;(newline)
;(display "debug show2")
;(newline)
;        (display (string-join (append (list input) other) " "))
        (display input)
        (newline))
    (define (gl n (volume (get-gain)))
        (* (log (+ 0.5 (gh n))) volume)
    )
    (define (reverse-string value-end)
        (list->string (reverse (string->list value-end)))
    )
    (define (vector-of value)
        (vector value value value))    
    (define (gh2 freq (volume (get-gain)))
        (* (gh freq) volume))
    (define (opa-cross input)
        (if (< input 0.5)
            (* 2 input)
            1
        )
    )

    (define Volume 1)
    (define (get-gain)
        Volume
    )
    (define (set-gain! volume)
        (set! Volume volume)
        (gain (exp volume))
    )

    (define-struct tps_elmt (name (time_pos #:mutable) (speed #:mutable)))
    (define-struct temps (tps_elmts) #:mutable)
    (define tps (make-temps (list (make-tps_elmt "1" 0 10))))
    (define (tempo name (speed null) (position null))
        (let
            (
                (id
                    (for/first ((i (build-list (length (temps-tps_elmts tps)) values))
                           #:when (equal? name (tps_elmt-name (list-ref (temps-tps_elmts tps) i))))
                        (let ((elmt (list-ref (temps-tps_elmts tps) i)))
                            (unless (null? speed)
                                (set-tps_elmt-speed! elmt speed))
                            (set-tps_elmt-time_pos! elmt (+ (tps_elmt-time_pos elmt) (* (delta) (tps_elmt-speed elmt))))
                            (unless (null? position)
                                (set-tps_elmt-time_pos! elmt position))
                            (tps_elmt-time_pos elmt)
                        )
                    )
                )
            )
            (cond (id
                id)
                (else
                    (set-temps-tps_elmts! tps (append (temps-tps_elmts tps) (list (make-tps_elmt name 0 speed))))
                    (tempo name speed position)
                )
            )
        )
    )
    (define travels (make-hash))
    (define-struct trip (start end speed time0))
    (define
        (travel
            id
            start
            end
            speed
;            time0
        )
;(show (hash-ref travels id #f))
        (let*
            (
                (dt (min (/ (- (time-now) (hash-ref! travels id (time-now))) speed) 1))
                (v
                    (vadd
                        start
                        (vmul
                            (vsub
                                end
                                start
                            )
                            dt
                        )
                    )
                )
            )
;            (when (= dt 1)
;                (hash-set! travels id (time-now))
;            )
            v
        )
    )
    (define (travel-end id)
        (hash-remove! travels id)
;(show (hash-has-key? travels id))
    )
    (define (defined? symbol)
        (let
            (
                (tested-symbol
                    (if (string? symbol)
                        (string->symbol symbol)
                        symbol
                    )
                )
            )
            (if (member tested-symbol (namespace-mapped-symbols))
                #t
                #f
            )
        )
    )
    (define (neg value)
        (* -1 value)
    )
    (define (degre->pi a)
        (* a (/ (* 2 pi) 360))
    )
    (define (rad->degre a)
        (/ (* 360 a) (* 2 pi))
    )
    (define (cylindric->cartesien c)
        (let
            (
                (p (vector-ref c 0))
                (g (vector-ref c 1))
                (z (vector-ref c 2))
            )
            (vector
                (* p (cos (degre->pi g)))
                (* p (sin (degre->pi g)))
                z
            )
        )
    )
    (define (cylindric-rad->cartesien c)
        (let
            (
                (p (vector-ref c 0))
                (g (vector-ref c 1))
                (z (vector-ref c 2))
            )
            (vector
                (* p (cos g))
                (* p (sin g))
                z
            )
        )
    )
    (define (cartesien->cylindric c)
        (let*
            (
                (x (vector-ref c 0))
                (y (vector-ref c 1))
                (z (vector-ref c 2))
    
                (p (sqrt (+ (sqr x) (sqr y))))
                (g 
                    (cond
                        ((positive? x)
                            (atan (/ y x))
                        )
                        ((negative? x)
                            (cond
                                ((positive? y)
                                    (+ (atan (/ y x)) pi)
                                )
                                ((negative? y)
                                    (- (atan (/ y x)) pi)
                                )
                            )
                            
                        )
                        (else 0)
                    )
                )
            )
            (vector p g z)
        )
    )
    (define (cartesien->cylindric-deg c)
        (let*
            (
                (x (vector-ref c 0))
                (y (vector-ref c 1))
                (z (vector-ref c 2))
    
                (p (sqrt (+ (sqr x) (sqr y))))
                (g (rad->degre (acos (/ x p))))
            )
            (vector p g z)
        )
    )
    (define (find v listing)
        (findf
            (lambda (arg)
                (equal? arg v)
            )
            listing
        )
    )
    (define (vector-non-empty? v)
        (for/first ((i (vector->list v)) #:when (positive? i))
            #t
        )
    )
    (define (defil-v axis v #:v-init (v-init 0) #:v-max (v-max 10) #:v-min (v-min -30))
        (cond
            ((>= (vector-ref v axis) v-max)
                (vector-set! v axis v-init)
            )
            ((<= (vector-ref v axis) v-min)
                (vector-set! v axis v-init)
            )
        )
        v
    )
    (define (defil-x v #:v-init (v-init 0) #:v-max (v-max 20) #:v-min (v-min -20))
        (defil-v 0 v #:v-init v-init #:v-max v-max #:v-min v-min)
    )
    (define (defil-y v #:v-init (v-init 0) #:v-max (v-max 10) #:v-min (v-min -30))
        (defil-v 1 v #:v-init v-init #:v-max v-max #:v-min v-min)
    )
    (define (defil-z v #:v-init (v-init 0) #:v-max (v-max 10) #:v-min (v-min -10))
        (defil-v 2 v #:v-init v-init #:v-max v-max #:v-min v-min)
    )
    (define (defil v #:init (v-init 0) #:max (v-max 10) #:min (v-min -30))
        (cond
            ((>= v v-max)
                v-init
            )
            ((<= v v-min)
                v-init
            )
            (else v)
        )
    )
    (define (modulo-d v d)
        (if (zero? d)
            0
            (let*
                (
                    (v-p (* (expt 10 20) v))
                    (d-p (* (expt 10 20) d))
                )        
                (* (expt 10 -20) (modulo v-p d-p))
            )
        )
    )
)
