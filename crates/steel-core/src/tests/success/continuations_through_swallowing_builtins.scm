;; A continuation called from inside a builtin's callback leaves the builtin,
;; whatever the builtin does with the error that carries it out.

(define (escapes thunk-taking-k)
  (call/cc thunk-taking-k))

;; A reducer that keeps only some of what it is given
(assert! (equal? 'esc
                 (escapes (lambda (k)
                            (transduce '(1 2 3) (mapping (lambda (x) (if (= x 2) (k 'esc) x))) (into-last))))))
(assert! (equal? 'esc
                 (escapes (lambda (k)
                            (transduce '(1 2 3) (mapping (lambda (x) (if (= x 2) (k 'esc) x))) (into-count))))))
(assert! (equal? 'esc
                 (escapes (lambda (k)
                            (transduce '(1 2 3) (mapping (lambda (x) (if (= x 2) (k 'esc) x))) (into-nth 2))))))
(assert! (equal?
          'esc
          (escapes
           (lambda (k)
             (transduce '(1 2 3 4) (mapping (lambda (x) (if (= x 2) (k 'esc) x))) (dropping 2) (into-list))))))

;; A reducer that calls another function before it looks at the last result
(assert! (equal? 'esc
                 (escapes (lambda (k)
                            (transduce '(1 2 3)
                                       (mapping (lambda (x) x))
                                       (into-reducer (lambda (acc x) (if (= x 2) (k 'esc) (+ acc x))) 0))))))
(assert! (equal? 'esc
                 (escapes (lambda (k)
                            (transduce '(1 2 3)
                                       (mapping (lambda (x) x))
                                       (into-reducer (lambda (acc x) (if (= x 3) (k 'esc) (+ acc x))) 0))))))

;; The values around the call are the ones that were there
(define (around a b c)
  (list a b c))
(assert! (equal? '(a esc c)
                 (around 'a
                         (escapes (lambda (k)
                                    (transduce '(1 2 3)
                                               (mapping (lambda (x) (if (= x 2) (k 'esc) x)))
                                               (into-last))))
                         'c)))

;; An error some time later is its own, and goes to its own handler
(define times 0)
(define landed
  (escapes (lambda (k)
             (transduce '(1 2 3) (mapping (lambda (x) (if (= x 2) (k 'esc) x))) (into-last)))))
(set! times (+ times 1))
(assert! (equal? landed 'esc))
(assert! (equal? 'handled
                 (with-handler (lambda (e) 'handled)
                               (error "unrelated"))))
(assert! (= times 1))

;; An error in a callback, where the builtin gives one back, leaves the
;; values around the call alone
(assert! (equal? '(a handled c)
                 (around 'a
                         (with-handler (lambda (e) 'handled)
                                       (transduce '(1 2 3)
                                                  (mapping (lambda (x) (if (= x 2) (error "lost") x)))
                                                  (into-list)))
                         'c)))

;; A struct that can be called, whose procedure is itself, is not a function
(struct Loops (f) #:mutable #:prop:procedure 0)
(define loops (Loops 1))
(set-Loops-f! loops loops)
(assert! (equal? 'refused
                 (with-handler (lambda (e) 'refused)
                               (apply loops '(1)))))

;; A continuation of one thread is not one another thread can carry on from
(define from-thread #f)
(thread-join! (spawn-native-thread (lambda () (+ 1 (call/cc (lambda (k) (set! from-thread k) 1))))))
(assert! (equal? 'refused
                 (with-handler (lambda (e) 'refused)
                               (from-thread 5))))
