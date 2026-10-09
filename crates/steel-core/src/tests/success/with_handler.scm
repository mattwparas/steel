;; What `with-handler` gives back, which handler an error reaches, and what
;; has run by the time the handler does.

(define notes (box '()))
(define (note x)
  (set-box! notes (cons x (unbox notes))))
(define (noted)
  (let ([so-far (reverse (unbox notes))])
    (set-box! notes '())
    so-far))

(define (boom)
  (error "boom"))
;; An error some frames down, with additions waiting on the way back.
(define (deep n)
  (if (= n 0) (car 5) (+ 1 (deep (- n 1)))))
(define (one-argument x)
  x)
(define not-a-function 5)
(define (message e)
  (trim (error-object-message e)))

;; Without an error it is the body.
(assert! (= (with-handler (lambda (e) 'handled) 1) 1))
(assert! (= (with-handler (lambda (e) 'handled) (note 'a) (note 'b) 3) 3))
(assert! (equal? (noted) '(a b)))

;; With one it is what the handler returns, and the rest of the body is
;; left out.
(assert! (eq? (with-handler (lambda (e) 'handled) (boom)) 'handled))
(assert! (eq? (with-handler (lambda (e) 'handled) (note 'a) (boom) (note 'never)) 'handled))
(assert! (equal? (noted) '(a)))
(assert! (void? (with-handler (lambda (e) void) (boom))))
(assert! (eq? (with-handler (lambda (e) (note 'handler) 'handled) (note 'body) (boom)) 'handled))
(assert! (equal? (noted) '(body handler)))

;; Errors of every kind, from wherever they are raised.
(assert! (eq? (with-handler (lambda (e) 'handled) (car 5)) 'handled))
(assert! (eq? (with-handler (lambda (e) 'handled) (deep 50)) 'handled))
(assert! (eq? (with-handler (lambda (e) 'handled) (apply one-argument (list))) 'handled))
(assert! (eq? (with-handler (lambda (e) 'handled) ((car (list not-a-function)) 5)) 'handled))
(assert! (eq? (with-handler (lambda (e) 'handled) (hash-ref (hash 'a 1) 'b)) 'handled))
(assert! (eq? (with-handler (lambda (e) 'handled) (vector-ref (vector 1 2) 5)) 'handled))
(assert! (eq? (with-handler (lambda (e) 'handled) (+ 1 "a")) 'handled))
(assert! (eq? (with-handler (lambda (e) 'handled) (apply car (list 5))) 'handled))

;; The handler is given the error.
(assert! (with-handler (lambda (e) (error-object? e)) (boom)))
(assert! (equal? (with-handler (lambda (e) (message e)) (error "the message")) "the message"))

;; The handler can be any function, and is looked up when the error comes.
(define (by-name e)
  'by-name)
(assert! (eq? (with-handler by-name (boom)) 'by-name))
(assert! (eq? (let ([local (lambda (e) 'local)]) (with-handler local (boom))) 'local))

;; The nearest handler takes it, and is no longer there for what follows.
(assert! (eq? (with-handler (lambda (e) 'outer) (with-handler (lambda (e) 'inner) (boom))) 'inner))
(assert! (eq? (with-handler (lambda (e) 'outer)
                            (list (with-handler (lambda (e) 'inner) (boom)) (boom)))
              'outer))
(assert! (equal? (with-handler (lambda (e) 'outer)
                               (list (with-handler (lambda (e) 'first) (boom))
                                     (with-handler (lambda (e) 'second) (boom))))
                 '(first second)))

;; An error in a handler goes to the next one out.
(assert! (equal? (with-handler (lambda (e) (list 'outer (message e)))
                               (with-handler (lambda (e) (error "again")) (boom)))
                 '(outer "again")))
(assert! (eq? (with-handler (lambda (e) 'outer) (with-handler (lambda (e) (car 5)) (boom))) 'outer))
(assert! (equal? (with-handler (lambda (e) (list 'outer (message e)))
                               (with-handler (lambda (e) (raise-error e)) (error "passed on")))
                 '(outer "passed on")))
(assert! (equal? (with-handler
                  (lambda (e) (note 'third) (message e))
                  (with-handler (lambda (e) (note 'second) (raise-error e))
                                (with-handler (lambda (e) (note 'first) (raise-error e))
                                              (error "all the way"))))
                 "all the way"))
(assert! (equal? (noted) '(first second third)))

;; The result joins whatever was waiting for it.
(assert! (equal? (list 1 (with-handler (lambda (e) 2) (boom)) 3) '(1 2 3)))
(assert! (= (+ 1 (* 2 (with-handler (lambda (e) 10) (deep 5)))) 21))
(assert! (equal? (let ([a 1] [b (with-handler (lambda (e) 2) (boom))]) (list a b)) '(1 2)))
(define at-the-top (with-handler (lambda (e) 'handled) (deep 3)))
(assert! (eq? at-the-top 'handled))

;; `dynamic-wind` leaves before the handler runs, when it is inside.
(assert! (= (dynamic-wind (lambda () (note 'in))
                          (lambda () (with-handler (lambda (e) 'handled) 1))
                          (lambda () (note 'out)))
            1))
(assert! (equal? (noted) '(in out)))
(assert! (eq? (with-handler (lambda (e) (note 'handler) 'handled)
                            (dynamic-wind (lambda () (note 'in)) (lambda () (boom)) (lambda () (note 'out))))
              'handled))
(assert! (equal? (noted) '(in out handler)))
(assert! (eq? (dynamic-wind (lambda () (note 'in))
                            (lambda () (with-handler (lambda (e) (note 'handler) 'handled) (boom)))
                            (lambda () (note 'out)))
              'handled))
(assert! (equal? (noted) '(in handler out)))
(assert! (eq? (with-handler (lambda (e) 'handled)
                            (dynamic-wind (lambda () (note 'in1))
                                          (lambda ()
                                            (dynamic-wind (lambda () (note 'in2))
                                                          (lambda () (boom))
                                                          (lambda () (note 'out2))))
                                          (lambda () (note 'out1))))
              'handled))
(assert! (equal? (noted) '(in1 in2 out2 out1)))

;; Parameters are back to what they were where the handler was installed.
(define depth (make-parameter 1))
(assert! (equal? (list (with-handler (lambda (e) (depth)) (parameterize ([depth 2]) (boom))) (depth))
                 '(1 1)))
(assert! (= (parameterize ([depth 5])
              (with-handler (lambda (e) (depth)) (parameterize ([depth 6]) (boom))))
            5))

;; Continuations in and out.
(assert! (eq? (call/cc (lambda (k) (with-handler (lambda (e) 'handled) (k 'escaped)))) 'escaped))
(assert! (eq? (call/cc (lambda (k) (with-handler (lambda (e) (k 'escaped)) (boom)))) 'escaped))
(assert! (= (with-handler (lambda (e) 'handled) (+ 1 (call/cc (lambda (k) (k 1))))) 2))
;; A handler passed over by an escape is not left installed.
(define (escape-then-raise)
  (with-handler (lambda (e) 'outer)
                (call/cc (lambda (k) (with-handler (lambda (e) 'passed-over) (k 1))))
                (boom)))
(assert! (eq? (escape-then-raise) 'outer))

;; It does not stand in for `reset`. What `shift` captures runs to the
;; `reset` around it.
(assert! (= (reset (+ 1 (with-handler (lambda (e) 10) (boom)))) 11))
(assert! (= (reset (+ 1 (with-handler (lambda (e) 10) (shift k (k 5))))) 6))
(assert! (= (reset (+ 1 (with-handler (lambda (e) 10) (shift k 5)))) 5))
(assert! (= (reset (+ 1 (with-handler (lambda (e) 10) (shift k (+ (k 1) (k 2)))))) 5))
(assert! (= (reset (+ 1 (with-handler (lambda (e) 10) (+ (shift k (k 5)) (boom))))) 11))

;; Errors under functions that call back in.
(define (unless-two x)
  (if (= x 2) (boom) x))
(assert! (eq? (with-handler (lambda (e) 'handled) (map unless-two (list 1 2 3))) 'handled))
(assert! (eq? (with-handler (lambda (e) 'handled) (for-each (lambda (x) (note x) (unless-two x)) (list 1 2 3)))
              'handled))
(assert! (equal? (noted) '(1 2)))
(assert! (eq? (with-handler (lambda (e) 'handled) (transduce (list 1 2 3) (mapping unless-two) (into-list)))
              'handled))
(assert! (eq? (with-handler (lambda (e) 'handled) (foldl (lambda (a b) (boom)) 0 (list 3 1 2))) 'handled))
(assert! (equal? (map (lambda (x) (with-handler (lambda (e) 0) (unless-two x))) (list 1 2 3)) '(1 0 3)))
(assert! (equal? (transduce (list 1 2 3)
                            (mapping (lambda (x) (with-handler (lambda (e) 0) (unless-two x))))
                            (into-list))
                 '(1 0 3)))

;; In a function called many times, with and without an error, so that it
;; is compiled along the way.
(define (many-errors n acc)
  (if (= n 0) acc (many-errors (- n 1) (+ acc (with-handler (lambda (e) 1) (boom))))))
(assert! (= (many-errors 20000 0) 20000))
(define (none n acc)
  (if (= n 0) acc (none (- n 1) (+ acc (with-handler (lambda (e) 1) 2)))))
(assert! (= (none 20000 0) 40000))
(define (every-other n acc)
  (if (= n 0)
      acc
      (every-other (- n 1) (+ acc (with-handler (lambda (e) 1) (if (even? n) (deep 3) 2))))))
(assert! (= (every-other 20000 0) 30000))
(define (made-again)
  (with-handler (lambda (e) 'handled) (boom)))
(assert! (equal? (list (made-again) (made-again)) '(handled handled)))

;; Handlers nested deeply, entered from the handler and from the body.
(define (down-through-handlers n)
  (if (= n 0) 'bottom (with-handler (lambda (e) (down-through-handlers (- n 1))) (boom))))
(assert! (eq? (down-through-handlers 2000) 'bottom))
(define passed (box 0))
(define (up-through-handlers n)
  (if (= n 0)
      (boom)
      (with-handler (lambda (e) (set-box! passed (+ 1 (unbox passed))) (raise-error e))
                    (up-through-handlers (- n 1)))))
(assert! (eq? (with-handler (lambda (e) 'top) (up-through-handlers 500)) 'top))
(assert! (= (unbox passed) 500))

;; Each thread has its own handlers, and an error that a thread does not
;; handle is raised where it is joined.
(assert! (eq? (thread-join! (spawn-native-thread (lambda () (with-handler (lambda (e) 'handled) (boom)))))
              'handled))
(assert! (eq? (with-handler (lambda (e) 'joined) (thread-join! (spawn-native-thread (lambda () (boom)))))
              'joined))
(define (handle-many n)
  (many-errors n 0))
(define handling
  (list (spawn-native-thread (lambda () (handle-many 5000)))
        (spawn-native-thread (lambda () (handle-many 5000)))
        (spawn-native-thread (lambda () (handle-many 5000)))))
(assert! (equal? (map thread-join! handling) '(5000 5000 5000)))
