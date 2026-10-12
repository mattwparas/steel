(define (escapes thunk-taking-k)
  (call/cc thunk-taking-k))

;; A reducer that drops some of its input
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

;; A reducer that calls another function first
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

;; Values around the call are kept
(define (around a b c)
  (list a b c))
(assert! (equal? '(a esc c)
                 (around 'a
                         (escapes (lambda (k)
                                    (transduce '(1 2 3)
                                               (mapping (lambda (x) (if (= x 2) (k 'esc) x)))
                                               (into-last))))
                         'c)))

;; A later error goes to its own handler
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

;; An error in a callback leaves the values around the call alone
(assert! (equal? '(a handled c)
                 (around 'a
                         (with-handler (lambda (e) 'handled)
                                       (transduce '(1 2 3)
                                                  (mapping (lambda (x) (if (= x 2) (error "lost") x)))
                                                  (into-list)))
                         'c)))

;; A callable struct whose procedure is itself
(struct Loops (f) #:mutable #:prop:procedure 0)
(define loops (Loops 1))
(set-Loops-f! loops loops)
(assert! (equal? 'refused
                 (with-handler (lambda (e) 'refused)
                               (apply loops '(1)))))

;; A continuation from another thread
(define from-thread #f)
(thread-join! (spawn-native-thread (lambda () (+ 1 (call/cc (lambda (k) (set! from-thread k) 1))))))
(assert! (equal? 'refused
                 (with-handler (lambda (e) 'refused)
                               (from-thread 5))))
