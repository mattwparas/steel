(require-builtin steel/time)
(require "steel/result")

(provide spawn-cancellable-thread-looping)

;; Spawns a thread, returning a handle to the sender to that thread.
; (define (message-passing)
;   (define channels (make-channels))
;   (define sender (list-ref channels 0))
;   (define receiver (list-ref channels 1))

;   ;; Worker thread, listen to requests
;   (spawn-thread! (lambda ()
;                    ;; Process incoming requests.
;                    (while #true (displayln (channel->recv receiver)))
;                    (loop)))

; sender)

(define *CHILD_THREADS* '())

;; Keep track of all of the threads currently running
(define (record-thread-handle handle)
  (set! *CHILD_THREADS* (cons handle *CHILD_THREADS*)))

(struct CancellableThreadHandle (sender handle))

;;@doc
;; Spawn a function, func, that runs on a background thread, running at the interval `delay-ms`
(define (spawn-cancellable-thread-looping func delay-ms)
  ;; Create the channels. We're going to cancel the thread using
  ;; the sender here to interrupt the receiver
  (define channels (make-channels))
  (define sender (list-ref channels 0))
  (define receiver (list-ref channels 1))

  (CancellableThreadHandle sender
                           (spawn-thread! (lambda ()
                                            (while (not (~> (channel->try-recv receiver) (unwrap-ok)))
                                                   (begin
                                                     (func)
                                                     (time/sleep-ms delay-ms)))
                                            (stdout-simple-displayln "Shutting down thread: "
                                                                     (thread::current/id))))))

; (let ([tasks (map (lambda (_)
;                     (spawn-thread! (lambda ()
;                                      (time/sleep-ms 2000)
;                                      (displayln (thread::current/id))
;                                      1)))
;                   (range 0 10))])
;   (displayln (map thread-join! tasks)))
