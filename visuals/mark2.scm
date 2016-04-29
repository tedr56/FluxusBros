(define (draw-leaf line t0 t1 start end task-name freq)
    (let*
        (
            (delta-time (- t1 t0))
            (elapsed-time (- (time-now) t0))
            (delta-f0 0.25)
            (delta-f1 0.75)
            (delta-f  (- (- 1 delta-f0) (- 1 delta-f1)))
            (delta-time-f0 (* delta-time delta-f0))
            (delta-time-f1 (* delta-time (- 1 delta-f1)))
            (delta-time-f  (* delta-time delta-f))
            (elapsed-time-f0 elapsed-time)
            (elapsed-time-f1 (- (time-now) (- t1 delta-time-f1)))
            (elapsed-time-f  (- (time-now) (+ t0 delta-time-f0)))
            (progress-f0 (/ elapsed-time-f0 delta-time-f0))
            (progress-f1 (/ elapsed-time-f1 delta-time-f1))
            (progress-f  (/ elapsed-time-f  delta-time-f))
            (progress (/ elapsed-time delta-time))
            )
        (with-primitive line
                (hint-none)
                (hint-wire)
                #(hint-unlit)
                (hint-wire-stippled)
                (line-pattern 4 #x0aaaa)
                
                #(line-width (inexact->exact (round (* (gh freq) (mn 7 7 10)))))
                (line-width (inexact->exact (round (mn 5 7 10))))
                (hint-vertcols)
                (pdata-set! "p" 0 start)
                (pdata-set! "p" 1 end)
                (if (< progress delta-f0)
                    (pdata-set! "c" 0 (hsv->rgb (vector .2 1 progress-f0)))
                    (pdata-set! "c" 0 (hsv->rgb (vector .2 1 (- 1 progress-f))))
                    )
                (if (> progress delta-f1)
                    (pdata-set! "c" 1 (hsv->rgb (vector .3 1 (- 1 progress-f1))))
                    (pdata-set! "c" 1 (hsv->rgb (vector .3 1 progress-f)))
                    )
                #(show "")
                #(show t0)
                #(show delta-time-f1)
                #(show elapsed-time-f1)
                #(show progress)
                #(show progress-f1)
                
            )
        (when (>= progress 1)
            (destroy line)
            (rm-task task-name)
            )
        )
    )

(define preTime (time-now))
(define (mark2-anim n)
    (when (> (- (time-now) preTime) (mn 0 7 2))
        (let*
            (
                (task-name (gensym))
                (t0 (time-now))
                (speed (+ (mn 1 7 3) .001))
                (XsizeS (* (random-center 20) (mn 2 7 20)))
                (YsizeS (* (random-center 20) (mn 3 7 20)))
                (ZsizeS (* (random-center 20) (mn 4 7 20)))
                (XsizeE (* (random-center 20) (mn 2 7 20)))
                (YsizeE (* (random-center 20) (mn 3 7 20)))
                (ZsizeE (* (random-center 20) (mn 4 7 20)))
                (Yoffset (- (mn 5 7 30) 15))
                (start (vector XsizeS (+ YsizeS Yoffset) ZsizeS))
                (end   (vector XsizeE (+ YsizeE Yoffset) ZsizeE))
                (freq n)
                (leaf (build-ribbon 2))
                )
            (spawn-task (lambda () (draw-leaf leaf t0 (+ t0 speed) start end task-name freq)) task-name)
            )
        (unless (< n 0)
            (mark2-anim (- n 1)))
        (set! preTime (time-now))
        )
    
    )

(define (mark2 id cross)
    (mark2-anim (mn 6 7 16))
)

