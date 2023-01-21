; *thread-queue* : list[continuation]
(define *thread-queue* '())

; halt : continuation
(define halt #f)

; current-continuation : -> continuation
(define (current-continuation)
  (call/cc
   (lambda (cc)
     (cc cc))))

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
    ; (displayln cc)
    (if cc
        (begin
        ;   (displayln cc)
          (set! halt (lambda () 
                        ; (inspect-bytecode cc) 
                        ; (displayln cc)
                        (cc #f)))
        ;   (displayln cc)
          (if (null? *thread-queue*)
              void
              (begin
                (let ((next-thread (car *thread-queue*)))
                  (set! *thread-queue* (cdr *thread-queue*))
                  (next-thread 'resume)))))
        void)))


;; Example cooperatively threaded program
(define counter 10)

(define (make-thread-thunk name)
  (define (loop)
        (when (< counter 0)
            (quit))
        (display "in thread ")
        (display name)
        (display "; counter = ")
        (display counter)
        (newline)
        (set! counter (- counter 1))
        (yield)
        (loop))
  loop)

(spawn (make-thread-thunk 'a))
(spawn (make-thread-thunk 'b))
(spawn (make-thread-thunk 'c))

(start-threads)
