(require-builtin dylib/steel/webserver)

(define vm-receiver void)
(define vm-sender void)
(define command-channel void)

;; We don't need the extra garbage laying around, so we'll just go ahead
;; and unroot these channels here
(let ((channels (setup-channels)))
    (set! vm-sender (first channels))
    (set! vm-receiver (second channels))
    (set! command-channel (third channels)))


(define (handle-request path)
    (displayln path)
    "hello world!")

(define (loop)
    (->> (receiver/recv vm-receiver)
            (handle-request)
            (sender/send vm-sender))

    (loop))


(start-server! command-channel)

(loop)