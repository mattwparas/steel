;; Frames that an error leaves by. A continuation captured in one and kept
;; has to be whole when it is called later, and a handler further out has to
;; see an error raised under a builtin that called back in.

(define (handled thunk)
  (call-with-exception-handler (lambda (e) 'handled) thunk))

;; Captured, then the frame is left by an error, then called twice more.
(define kept (box #f))
(define trips (box 0))
(define seen (box '()))
(define (capture-then-raise)
  (+ 1 (call/cc (lambda (k) (set-box! kept k) (error "left by an error")))))
(define (reenter)
  (let ([v (handled (lambda () (capture-then-raise)))])
    (set-box! trips (+ 1 (unbox trips)))
    (set-box! seen (cons v (unbox seen)))
    (if (< (unbox trips) 3) ((unbox kept) 10) 'done)))
(assert! (eq? (reenter) 'done))
(assert! (equal? (unbox seen) '(11 11 handled)))

;; The same with the error raised some frames below the capture.
(define (raise-below depth)
  (if (= depth 0) (car 5) (+ 1 (raise-below (- depth 1)))))
(define (capture-then-raise-below)
  (+ 1 (call/cc (lambda (k) (set-box! kept k) (raise-below 5)))))
(define (reenter-below)
  (let ([v (handled (lambda () (capture-then-raise-below)))])
    (set-box! trips (+ 1 (unbox trips)))
    (set-box! seen (cons v (unbox seen)))
    (if (< (unbox trips) 3) ((unbox kept) 20) 'done)))
(set-box! trips 0)
(set-box! seen '())
(assert! (eq? (reenter-below) 'done))
(assert! (equal? (unbox seen) '(21 21 handled)))

;; The handler raises again, so two handler frames are left by errors.
(define (reenter-through-two)
  (let ([v (handled (lambda ()
                      (call-with-exception-handler (lambda (e) (error "again"))
                                                   (lambda () (capture-then-raise)))))])
    (set-box! trips (+ 1 (unbox trips)))
    (set-box! seen (cons v (unbox seen)))
    (if (< (unbox trips) 3) ((unbox kept) 30) 'done)))
(set-box! trips 0)
(set-box! seen '())
(assert! (eq? (reenter-through-two) 'done))
(assert! (equal? (unbox seen) '(31 31 handled)))

;; The same through `with-handler`, whose body is entered again.
(define (reenter-with-handler)
  (let ([v (with-handler (lambda (e) 'handled) (capture-then-raise))])
    (set-box! trips (+ 1 (unbox trips)))
    (set-box! seen (cons v (unbox seen)))
    (if (< (unbox trips) 3) ((unbox kept) 40) 'done)))
(set-box! trips 0)
(set-box! seen '())
(assert! (eq? (reenter-with-handler) 'done))
(assert! (equal? (unbox seen) '(41 41 handled)))

;; A handler's result joins operands that were waiting for it.
(assert! (equal? (list 1 (handled (lambda () (+ 1 (raise-below 2))))) '(1 handled)))
(assert! (equal? (list 1 (with-handler (lambda (e) 2) (+ 1 (raise-below 2))) 3) '(1 2 3)))

;; Captured and not kept. Nothing is copied, however often.
(define (capture-and-drop)
  (+ 1 (call/cc (lambda (k) (error "dropped")))))
(define (drop-many n caught)
  (if (= n 0)
      caught
      (drop-many (- n 1)
                 (if (eq? 'handled (handled (lambda () (capture-and-drop)))) (+ caught 1) caught))))
(assert! (= (drop-many 20000 0) 20000))

;; Escaping upward past frames that have handlers.
(define (escape-past-handlers)
  (+ 1
     (call/cc (lambda (k)
                (with-handler (lambda (e) 'not-reached)
                              (with-handler (lambda (e) 'not-reached) (k 10)))))))
(assert! (= (escape-past-handlers) 11))

;; An error under a builtin that called back in reaches the handler outside.
(define (under-transduce)
  (with-handler (lambda (e) 'caught)
                (transduce (list 1 2 3) (mapping (lambda (x) (error "in a callback"))) (into-list))))
(assert! (eq? (under-transduce) 'caught))
(define (under-transduce-directly)
  (handled (lambda ()
             (transduce (list 1 2 3) (mapping (lambda (x) (car x))) (into-list)))))
(assert! (eq? (under-transduce-directly) 'handled))
(define (under-two-builtins)
  (with-handler
   (lambda (e) 'caught)
   (transduce (list 1 2 3)
              (mapping (lambda (x)
                         (transduce (list x) (mapping (lambda (y) (error "deeper"))) (into-list))))
              (into-list))))
(assert! (eq? (under-two-builtins) 'caught))
;; And the builtin still works afterwards.
(define (after-an-error)
  (transduce (list 1 2 3) (mapping (lambda (x) (* 2 x))) (into-list)))
(assert! (equal? (after-an-error) '(2 4 6)))
(define (caught-inside)
  (transduce (list 1 2 3)
             (mapping (lambda (x) (with-handler (lambda (e) 0) (if (= x 2) (error "two") x))))
             (into-list)))
(assert! (equal? (caught-inside) '(1 0 3)))
