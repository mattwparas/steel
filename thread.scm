(require-builtin steel/time)
(require "steel/result")


(define (loop thread-id)
  (displayln "Hello world!")
  (time/sleep-ms 500)
  (displayln "Hello world from: " thread-id)
  (loop thread-id))


(define (run-background-loop)
  (displayln "Hello world!")
  ; (displayln spawn-thread!)
  (let ((handle (spawn-thread!
                  (λ ()
                    (loop 1))))
        (handle2 (spawn-thread!
                   (λ ()
                     (time/sleep-ms 250)
                     (loop 2)))))
    (thread-join! handle2)))

; (spawn-thread! (lambda () (run-background-loop)))

; (let ((handle (spawn-thread! (λ () 
;                   (+ 10 (list-ref (list "hello world") 0))))))
;   (time/sleep-ms 5000)
;   (unwrap-ok
;     (thread-join! handle)))

;; Spawn thread! -> This requires some up front overhead, however spawning a new thread
;; takes about 400 microseconds, which isn't ideal. In theory we do not need to deep clone _everything_,
;; but there are certain optimizations that need to be implemented up front for this to work.
(thread-join!
  (spawn-thread! run-background-loop))

