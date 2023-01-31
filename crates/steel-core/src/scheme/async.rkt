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