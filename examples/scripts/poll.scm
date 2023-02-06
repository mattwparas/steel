; await : future -> value
; yield the current thread and loop until the value is completed

;; How to make the contract compile the exact same as a normal function?
; (define/contract (block-on future)
;   (->/c future? any/c)
;   (define (loop future)
;     (define output (poll! future))
;     (if (equal? #f output)
;         ;; Here, we would yield to the executor if we were await-ing
;         ;; but we're going to just block until the future is complete
;         (loop future)
;         output))
;   (loop future))


; (define (block-on future)
;   ; (->/c future? any/c)
;     (define output (poll! future))
;     (if (equal? #f output)
;         ;; Here, we would yield to the executor if we were await-ing
;         ;; but we're going to just block until the future is complete
;         (block-on future)
;         output))

(define (test) 10)


(define (block-on future)
  ; (->/c future? any/c)
    (define output (poll! future))
    (if (equal? #f output)
        ;; Here, we would yield to the executor if we were await-ing
        ;; but we're going to just block until the future is complete
        (block-on future)
        output))

(define block-on 
  (bind/c
     (make-function/c
        (make/c future? 'future?)
        (make/c any/c 'any/c))
      block-on
      'block-on))


(define (main)
    (displayln "Starting the main (single) thread now!")
    (define fut-1 (test))
    (define fut-2 (test))
    (define fut-3 (test))
    (displayln "Made it to here!")
    ;; we're just going to block on the three different async calls
    (block-on 
        (join! fut-1 fut-2 fut-3))
    ; (displayln "testing")
    
    )

; (main)

; (define main
;   (λ ()
;     ((λ (#####define-conversion0 fut-1 fut-2 fut-3)
;          ((λ (#####define-conversion0
;                 #####fut-11
;                 #####fut-22
;                 #####fut-33)
;               (begin
;                (set! fut-1 #####fut-11)
;                 (set! fut-2 #####fut-22)
;                 (set! fut-3 #####fut-33)
;                 (displayln "Made it to here!")
;                 (block-on (join! fut-1 fut-2 fut-3))))
;             (displayln
;                "Starting the main (single) thread now!")
;             (test)
;             (test)
;             (test)))
;        123
;        123
;        123
;        123)))