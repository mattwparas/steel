; *thread-queue* : list[continuation]
(define *thread-queue* '())

; halt : continuation
(define halt #f)

; current-continuation : -> continuation
(define (current-continuation)
  (call/cc
   (lambda (cc)
     (cc cc))))

; await : future -> value
; yield the current thread and loop until the value is completed
(define (await future)
    (define output (poll! future))
    ; (displayln output)
    (if output
        output
        (begin
            (yield)
            (await future))))

; spawn : (-> anything) -> void
(define (spawn thunk)
  (let ((cc (current-continuation)))
    (if (continuation? cc)
        (set! *thread-queue* (append *thread-queue* (list cc)))
        (begin 
               (thunk)
               (quit)))))

; yield : value -> void
(define (yield)
  (let ((cc (current-continuation)))
    (if (and (continuation? cc) (pair? *thread-queue*))
        (let ((next-thread (car *thread-queue*)))
          (set! *thread-queue* (append (cdr *thread-queue*) (list cc)))
          (next-thread 'resume))
        void)))

; quit : -> ...
(define (quit)
  (if (pair? *thread-queue*)
      (let ((next-thread (car *thread-queue*)))
        (set! *thread-queue* (cdr *thread-queue*))
        (next-thread 'resume))
      (halt)))
   
; start-threads : -> ...
(define (start-threads)
  (let ((cc (current-continuation)))
    (if cc
        (begin
          (set! halt (lambda () (cc #f)))
          (if (null? *thread-queue*)
              void
              (let ((next-thread (car *thread-queue*)))
                    (set! *thread-queue* (cdr *thread-queue*))
                    (next-thread 'resume))))
        void)))

;; Define an entry point 
(define-syntax async-main
  (syntax-rules ()
    [(async-main main-func)
    (begin main-func (spawn main) (start-threads))]))

;; Use a macro to make it a little bit prettier
(async-main
  (define (main)
      (displayln "Starting the main thread now!")
      (define fut-1 (test))
      (define fut-2 (test))
      (define fut-3 (test))
      ;; we're just going to block on the three different async calls
      (await 
          (join! fut-1 fut-2 fut-3))))

;; Alternatively, we could just do this normally
; (define (main)
;     (displayln "Starting the main thread now!")
;     (define fut-1 (test))
;     (define fut-2 (test))
;     (define fut-3 (test))
;     ;; we're just going to block on the three different async calls
;     (await 
;         (join! fut-1 fut-2 fut-3)))
; ;; Start one 'thread' for main
; (spawn main)

; ;; Kick off the execution
; (start-threads)
