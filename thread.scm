(require-builtin steel/time)


(define (loop thread-id)
  (time/sleep-ms 500)
  (displayln "Hello world from: " thread-id)
  (loop thread-id))


(let ((handle (spawn-thread! 
                (λ ()
                  (loop 1))))
      (handle2 (spawn-thread!
                 (λ ()
                   (time/sleep-ms 250)
                   (loop 2)))))

  (thread-join! handle2))


