;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SchemeTUIO Client Library (0.2)                                            ;;
;; Copyright (c) 2009-2010 Eren Güven <erenguven@cs.bilgi.edu.tr>             ;;
;;                                                                            ;;
;; This program is free software: you can redistribute it and/or modify       ;;
;; it under the terms of the GNU General Public License as published by       ;;
;; the Free Software Foundation, either version 3 of the License, or          ;;
;; (at your option) any later version.                                        ;;
;;                                                                            ;;
;; This program is distributed in the hope that it will be useful,            ;;
;; but WITHOUT ANY WARRANTY; without even the implied warranty of             ;;
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              ;;
;; GNU General Public License for more details.                               ;;
;;                                                                            ;;
;; You should have received a copy of the GNU General Public License          ;;
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang scheme
(require scheme/udp)

(provide (struct-out TuioPoint)(struct-out 2Dobj)(struct-out 2Dcur)
         start-tuio-client
         get-tuio-cursors
         get-tuio-objects
         )

;;; STRUCTURES ;;;

; sID: integer (session ID, unique)
; symID: integer (symbol ID, for objects)
; posX, posY: float [0,1] (position)
; angle: float [0,2PI] (angle)
; vecX, vecY: float (velocity vector)
; rotVec: float (rotation velocity vector)
; movAcc: float (movement acceleration)
; rotAcc: float (rotation acceleration)
; path: list-of-TuioPoint
(define-struct 2Dcur (sID posX posY vecX vecY movAcc path))
(define-struct 2Dobj (sID symID posX posY angle vecX vecY rotVec movAcc rotAcc path))
(define-struct TuioPoint (posX posY))

; Main Cursor & Object lists of client
(define TuioCursorList empty)
(define TuioObjectList empty)
; udp-socket
(define LISTENER (udp-open-socket))

; get-tuio-cursors: -> list-of-2Dcur
(define (get-tuio-cursors)
  TuioCursorList)
; get-tuio-objects: -> list-of-2Dobj
(define (get-tuio-objects)
  TuioObjectList)

; start-tuio-client: string integer -> initiates TuioClient
; ip: host-ip
; port: port @ host
;
; binds listener, starts a loop for updating main Cursor/Object lists, which are accessable anytime
; NOTE: thread needs to be killed manually (Ctrl-K in DrScheme or see demo file for example)
(define (start-tuio-client ip port)
  (udp-bind! LISTENER ip port) ; bind listener
  (thread tuio-r) ; initiate listen loop
  )

; after initiation, start a loop (recursion with no termination condition)
; reads at specified udp port, updates Cursor/Object lists on new datagram
(define (tuio-r)
  (begin
  (define TEMP-RECEIVE (make-bytes 1536 48))
  (set! TEMP-RECEIVE (make-bytes 1536 48))
  (udp-receive!* LISTENER TEMP-RECEIVE)
  (update-tuio-lists (extract-controller-type TEMP-RECEIVE)
                     (get-alive-ids TEMP-RECEIVE)
                     (get-objects TEMP-RECEIVE))
  (tuio-r) ; call self
  ))

; get-2Dcur: list-of-2Dcur integer -> 2Dcur (or false)
; returns 2Dcur with specified sID from list-of-2Dcur
(define (get-2Dcur a-2Dcur-list a-2Dcur-sID)
  (cond
    [(empty? a-2Dcur-list) false]
    [(= a-2Dcur-sID (2Dcur-sID (first a-2Dcur-list))) (first a-2Dcur-list)]
    [else (get-2Dcur (rest a-2Dcur-list) a-2Dcur-sID)] ))

; 2Dcur (or false) 2Dcur (or false) -> list-of-2Dcur (of length 1, list return choice due to single return type need)
; old2Dcur is from previous TuioCursorList (might not be there)
; new2Dcur is from last received TUIO-bundle (udp)
; 
(define (update-2Dcur old2Dcur new2Dcur)
  (cond
    [(and (2Dcur? old2Dcur) (2Dcur? new2Dcur))
     ; if both present, prepend old one's path to new one's and return new one
     (list (make-2Dcur (2Dcur-sID new2Dcur) (2Dcur-posX new2Dcur) (2Dcur-posY new2Dcur)
                 (2Dcur-vecX new2Dcur) (2Dcur-vecY new2Dcur) (2Dcur-movAcc new2Dcur)
                 (append (2Dcur-path old2Dcur) (2Dcur-path new2Dcur))))]
    [(2Dcur? new2Dcur) (list new2Dcur)] ; old is not present (new addition)
    [(2Dcur? old2Dcur) (list old2Dcur)] ; new is not present (active cursor, no update on status)
    [else empty] ))

; update-cursor-list: list-of-integers list-of-2Dcur -> void
; using alive-id-list
; keeps old Cursors (alive id present, no status update)
; updates Cursors
; adds new Cursors
(define (update-cursor-list alive-id-list new-cursor-list)
  (local ((define NEW-LIST empty)
          (define (helper id-list)
            (cond
              [(empty? id-list) (set! TuioCursorList NEW-LIST)]
              [else
               (set! NEW-LIST (append NEW-LIST (update-2Dcur (get-2Dcur TuioCursorList (first id-list))
                                                                   (get-2Dcur new-cursor-list (first id-list)))))
                           (helper (rest id-list))] )))
    (helper alive-id-list)))


; Object versions are the same as Cursor with the exception of save location (TuioObjectList)
(define (get-2Dobj a-2Dobj-list a-2Dobj-sID)
  (cond
    [(empty? a-2Dobj-list) false]
    [(= a-2Dobj-sID (2Dobj-sID (first a-2Dobj-list))) (first a-2Dobj-list)]
    [else (get-2Dobj (rest a-2Dobj-list) a-2Dobj-sID)] ))

(define (update-2Dobj old2Dobj new2Dobj)
  (cond
    [(and (2Dobj? old2Dobj) (2Dobj? new2Dobj))
     ; if both present, prepend old one's path to new one's and return new one
     (list (make-2Dobj (2Dobj-sID new2Dobj) (2Dobj-symID new2Dobj) (2Dobj-posX new2Dobj) (2Dobj-posY new2Dobj)
                       (2Dobj-angle new2Dobj) (2Dobj-vecX new2Dobj) (2Dobj-vecY new2Dobj) (2Dobj-rotVec new2Dobj)
                       (2Dobj-movAcc new2Dobj) (2Dobj-rotAcc new2Dobj)
                       (append (2Dobj-path old2Dobj) (2Dobj-path new2Dobj))))]
    [(2Dobj? new2Dobj) (list new2Dobj)] ; old is not present (new addition)
    [(2Dobj? old2Dobj) (list old2Dobj)] ; new is not present (active object, no update on status)
    [else empty] ))

; update-object-list: list-of-integers list-of-2Dobj -> void
; using alive-id-list
; keeps old Objects (alive id present, no status update)
; updates Objects
; adds new Objects
(define (update-object-list alive-id-list new-object-list)
  (local ((define NEW-LIST empty)
          (define (helper id-list)
            (cond
              [(empty? id-list) (set! TuioObjectList NEW-LIST)]
              [else
               (set! NEW-LIST (append NEW-LIST (update-2Dobj (get-2Dobj TuioObjectList (first id-list))
                                                             (get-2Dobj new-object-list (first id-list)))))
                           (helper (rest id-list))] )))
    (helper alive-id-list)))



; update-tuio-lists: byte-string list-of-integers list-of-tuio -> void
; update global lists
; each tuio-bundle has single profile (either 2Dcur or 2Dobj)
(define (update-tuio-lists controller-type id-list tuio-list)
  (cond
    [(equal? controller-type #"2Dcur\0") (update-cursor-list id-list tuio-list)]
    [(equal? controller-type #"2Dobj\0") (update-object-list id-list tuio-list)]
    ))


;;; from scheme-tuio-0.1 ;;;;;;;;;;;;;;;
;
; NOTES: Vector support (get-objects*) and 3Dobj are removed. These can be found in previous version.

;;;;; TIME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; bundle: byte-string received, TUIO bundle
; bytes[8,11]: seconds passed since yr. 1900, int32, unsigned, big_endian
; bytes[12,15]: fractal seconds, up to 10 decimal digits, int32, unsigned, big_endian (accuracy up to 400 ps)

; get-time-exact: byte-string -> number

(define (get-time-exact a-bundle)
  (+ (integer-bytes->integer (subbytes a-bundle 8 12) #f #t)
     (/ (integer-bytes->integer (subbytes a-bundle 12 16) #f #t) (expt 10 10))))

;;;;; ALIVE IDs ;;;;;;;;;;;;;;;;;;;;;;;;
; bundle: byte-string received, TUIO bundle

; get-alive-ids: byte-string -> list-of-numbers (IDs)
; count-alive: byte-string -> number
;     -counts the "i" (integer) amount (first message, after profile name, before "alive")
; list-alive: byte-string count
;     -captures and calculates int32 values (ID bytes)

(define (get-alive-ids a-bundle)
  (local ((define (list-alive a-bundle pointer count) ; pointer (1st int32) count(total)
            (cond
              [(equal? count 0) empty]
              [else (cons (integer-bytes->integer (subbytes a-bundle pointer (+ pointer 4)) #f #t)
                          (list-alive a-bundle (+ pointer 4) (sub1 count)))] ))
          (define (count-alive a-bundle pointer count)
            (cond
              [(equal? pointer (bytes-length a-bundle)) empty] ;; hms
              [(equal? (bytes-ref a-bundle pointer) 97) (list-alive a-bundle (+ pointer 8) count)] ; "a", skip alive[3]
              [(equal? (bytes-ref a-bundle pointer) 105) (count-alive a-bundle (add1 pointer) (add1 count))] ; "i"
              [else (count-alive a-bundle (add1 pointer) count)] )))
    (count-alive a-bundle 24 0)))   ;;;;

;;;;; CONTROLLER TYPE ;;;;;;;;;;;;;;;;;;
; 2Dcur 2Dobj 3Dobj

; bundle: byte-string received, TUIO bundle

; extract-controller-type: byte-string -> byte-string (example: #"2Dobj\0")
; captures "/tuio/" general tag and returns the 6 Bytes that follow (current TUIO spec.)
; "/tuio/******" 12bytes total (end gets filled with \0 as usual if necessary)

(define (extract-controller-type a-bundle)
  (local ((define (helper a-bundle pointer)
            (cond
              [(> pointer (- (bytes-length a-bundle) 12)) "fail"] ;;;;; no controller case
              [(equal? (subbytes a-bundle pointer (+ pointer 6)) #"/tuio/")
               (subbytes a-bundle (+ pointer 6) (+ pointer 12))]
              [else (helper a-bundle (add1 pointer))] )))
    (helper a-bundle 8))) ;;;;; skip bundle tag

;;;;; LENGTH INFO ;;;;;;;;;;;;;;;;;;;;;;
; helper function for 'find-all-set-messages'

; get-length-info: byte-string number number(0) number(0) number(0) -> list-of-numbers
;    - starts at "set-start", works backwards counting s-i-f markers, stopping at "," returning (list s i f)
; s,i,f stand for 'string', 'integer' and 'float' respectively

(define (get-length-info a-bundle pointer string-count integer-count float-count) ; pointer is @ #"set\0"
  (cond
    [(equal? (bytes-ref a-bundle pointer) 44) (list string-count integer-count float-count)] ; (,) end
    [(equal? (bytes-ref a-bundle pointer) 102) ; f
     (get-length-info a-bundle (sub1 pointer) string-count integer-count (add1 float-count))]
    [(equal? (bytes-ref a-bundle pointer) 105) ; i
     (get-length-info a-bundle (sub1 pointer) string-count (add1 integer-count) float-count)]
    [(and (equal? (bytes-ref a-bundle pointer) 115) ; s
          (not (equal? (bytes-ref a-bundle (add1 pointer)) 101))) ; e (against s of "set")
     (get-length-info a-bundle (sub1 pointer) (add1 string-count) integer-count float-count)]
    [else (get-length-info a-bundle (sub1 pointer) string-count integer-count float-count)] ))

;;;;; ALL SET MESSAGES ;;;;;;;;;;;;;;;;;
; helper function for 'get-all-set-details'

; find-all-set-messages: byte-string number -> list-of-lists (containing set index and length info)
; finds set message start points and combines them with their length info gathered from 'get-length-info'

(define (find-all-set-messages a-bundle pointer)
  (cond
    [(equal? pointer (- (bytes-length a-bundle) 12)) empty] ; no-controller
    [(equal? (subbytes a-bundle pointer (+ pointer 4)) #"fseq") empty] ; almost end of bundle
    [(equal? (subbytes a-bundle pointer (+ pointer 4)) #"set\0")
     (cons (append (list pointer) (get-length-info a-bundle pointer 0 0 0)) ; length info gets appended
           (find-all-set-messages a-bundle (+ pointer 20)))] ;;;;
    [else (find-all-set-messages a-bundle (add1 pointer))] ))

;;;;; SET DETAILS (single) ;;;;;;;;;;;;;
; helper function for 'get-all-set-details'

; get-set-details: byte-string number number number -> list-of-numbers (i f)
; uses length information gathered by "find-all-set-messages" (also get-length-info)

(define (get-set-details a-bundle pointer integer-left float-left) ; pointer is (+ #"set\0" 4) (aka. message start)
  (cond
    [(> integer-left 0)
     (cons (integer-bytes->integer (subbytes a-bundle pointer (+ pointer 4)) #f #t)
           (get-set-details a-bundle (+ pointer 4) (sub1 integer-left) float-left))]
    [(> float-left 0)
     (cons (floating-point-bytes->real (subbytes a-bundle pointer (+ pointer 4)) #t)
           (get-set-details a-bundle (+ pointer 4) integer-left (sub1 float-left)))]
    [else empty] ))

;;;;; SET DETAILS (all) ;;;;;;;;;;;;;;;;
; list representation of the objects (not structured)

; get-all-set-details: byte-string -> list of lists containing object/cursor information (values)
;    - uses index of set-message-start locations on byte-string, extracts every detail received

(define (get-all-set-details a-bundle)
  (local ((define (helper a-bundle list-of-all-set)
            (cond
              [(empty? list-of-all-set) empty]
              [else (cons (get-set-details a-bundle
                                           (+ (first (first list-of-all-set)) 4) ; (+ set-start 4) (get-set-details)
                                           (third (first list-of-all-set)) (fourth (first list-of-all-set)))
                                                   ; i f locations in list-of-all-set
                          (helper a-bundle (rest list-of-all-set)))] )) )

    (helper a-bundle (find-all-set-messages a-bundle 8)))) ;;;;; skip bundle tag

;;;;; FRAME ID ;;;;;;;;;;;;;;;;;;;;;;;;;
; frame id information which is sent with each bundle

; get-frame-id: byte-string -> number
;    - find-start-point: returns the byte index of #"fseq" start
;    - takes "fseq" start, skips "fseq" + 4Bytes fill, calculates the next (also last) int32

(define (get-frame-id a-bundle)
  (local ((define (find-start-point a-bundle pointer)
            (cond
              [(equal? (subbytes a-bundle pointer (+ pointer 4)) #"fseq") pointer]
              [else (find-start-point a-bundle (add1 pointer))] ))
          
          (define (calculate-id a-bundle pointer) ; pointer is #"fseq" start point
            (integer-bytes->integer (subbytes a-bundle (+ pointer 8) (+ pointer 12)) #f #t)))
            
    (calculate-id a-bundle (find-start-point a-bundle 8)))) ;;;; skip bundle tag


;;;;; OBJECTS/CURSORS ;;;;;;;;;;;;;;;;;;

; get-objects: byte-string -> list-of-objects (scheme structures)
; parses 'get-all-set-details' output to form a homogenous list-of-objects, where every element is a tuio-object

;(define-struct 2Dobj (id sym pos2D angle2D vector2D rot-vec2D mov-acc rot-acc))
;(define-struct 2Dcur (id pos2D vector2D mov-acc))
;(define-struct 3Dobj (id sym pos3D angle3D vector3D rot-vec3D mov-acc rot-acc))

(define (get-objects a-bundle)
  (local ((define CONTROLLER-TYPE (extract-controller-type a-bundle))
          (define BIG-LIST (get-all-set-details a-bundle))
          
          (define (helper-2Dcur a-list)
            (cond
              [(empty? a-list) empty]
              [else (cons (make-2Dcur (list-ref (first a-list) 0) ; id
                                      (list-ref (first a-list) 1) (list-ref (first a-list) 2) ; x, y
                                      (list-ref (first a-list) 3) (list-ref (first a-list) 4) ; X, Y
                                      (list-ref (first a-list) 5) ; m
                                      (list (make-TuioPoint (list-ref (first a-list) 1) (list-ref (first a-list) 2))))
                          (helper-2Dcur (rest a-list)))] ))
          
          (define (helper-2Dobj a-list)
            (cond
              [(empty? a-list) empty]
              [else (cons (make-2Dobj (list-ref (first a-list) 0) (list-ref (first a-list) 1) ; id, sym
                                      (list-ref (first a-list) 2) (list-ref (first a-list) 3) ; x, y (pos2D)
                                      (list-ref (first a-list) 4) ; a (angle2D)
                                      (list-ref (first a-list) 5) (list-ref (first a-list) 6) ; X, Y
                                      (list-ref (first a-list) 7) ; A (rot-vec2D)
                                      (list-ref (first a-list) 8)
                                      (list-ref (first a-list) 9)
                                      (list (make-TuioPoint (list-ref (first a-list) 2) (list-ref (first a-list) 3))))
                          (helper-2Dobj (rest a-list)))] ))
          
          )
    (cond
      [(equal? CONTROLLER-TYPE #"2Dcur\0") (helper-2Dcur BIG-LIST)]
      [(equal? CONTROLLER-TYPE #"2Dobj\0") (helper-2Dobj BIG-LIST)]
      [else empty] )))
