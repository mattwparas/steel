; await : future -> value
; yield the current thread and loop until the value is completed
(define/contract (block-on future)
  (->/c future? any/c)
  (define (loop future)
    (define output (poll! future))
    (if (equal? #f output)
        ;; Here, we would yield to the executor if we were await-ing
        ;; but we're going to just block until the future is complete
        (loop future)
        output))
  (loop future))


(define (main)
    (displayln "Starting the main (single) thread now!")
    (define fut-1 (test))
    (define fut-2 (test))
    (define fut-3 (test))
    ;; we're just going to block on the three different async calls
    (block-on 
        (join! fut-1 fut-2 fut-3)))

(main)