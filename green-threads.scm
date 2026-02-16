(provide spawn
         yield
         start-threads)

; *thread-queue* : list[continuation]
(define *thread-queue* '())

;; We can set up a dedicated event loop for listening to
;; events. When we actually get an event, we can re schedule
;; that specific cont to go back on to the thread queue.
;; Key -> cont
(define *async-events* (hash))

; halt : continuation
(define halt #f)

; current-continuation : -> continuation
(define (current-continuation)
  (call/cc (lambda (cc) (cc cc))))

; spawn : (-> anything) -> void
(define (spawn thunk)
  (let ([cc (current-continuation)])
    (if (continuation? cc)
        (set! *thread-queue* (append *thread-queue* (list cc)))
        (begin
          (thunk)
          (quit)))))

; async-ield : value -> void
(define (async-yield key)
  (let ([cc (current-continuation)])
    ;; If there is a continuation, and there is a thread queue?
    (if (and (continuation? cc) (pair? *thread-queue*))
        (let ([next-thread (car *thread-queue*)])
          ;; Put this key into the event loop
          (set! *async-events* (hash-insert *async-events* key cc))
          (next-thread 'resume))
        void)))

; yield : value -> void
(define (yield)
  (let ([cc (current-continuation)])
    (if (and (continuation? cc) (pair? *thread-queue*))
        (let ([next-thread (car *thread-queue*)])
          (set! *thread-queue* (append (cdr *thread-queue*) (list cc)))
          (next-thread 'resume))
        void)))

; quit : -> ...
(define (quit)
  (if (pair? *thread-queue*)
      (let ([next-thread (car *thread-queue*)])
        (set! *thread-queue* (cdr *thread-queue*))
        (next-thread 'resume))
      (halt)))

; start-threads : -> ...
(define (start-threads)
  (let ([cc (current-continuation)])
    (if cc
        (begin
          (set! halt (lambda () (cc #f)))
          (if (null? *thread-queue*)
              void
              (begin
                (let ([next-thread (car *thread-queue*)])
                  (set! *thread-queue* (cdr *thread-queue*))
                  (next-thread 'resume)))))
        void)))

;; Example cooperatively threaded program
; (define counter 10)

; (define (make-thread-thunk name)
;   (define (loop)
;     (when (< counter 0)
;       (quit))
;     (display "in thread ")
;     (display name)
;     (display "; counter = ")
;     (display counter)
;     (newline)
;     (set! counter (- counter 1))
;     (yield)
;     (loop))
;   loop)

; (spawn (make-thread-thunk 'a))
; (spawn (make-thread-thunk 'b))
; (spawn (make-thread-thunk 'c))

; (start-threads)
