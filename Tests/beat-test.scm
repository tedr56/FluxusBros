(require "beat-module.scm")

(define (beat-test)
    (when (beat-catch "1")
        (show "**********************************")
        (show (time))
    )
)

(every-frame (beat-test))