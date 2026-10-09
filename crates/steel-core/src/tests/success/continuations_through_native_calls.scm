;; Continuations and builtins that call back in, such as `transduce`. Such a
;; callback runs under native code. A continuation from outside it can be
;; called to leave, and one captured inside it cannot be called once the
;; builtin has moved on.

(define notes (box '()))
(define (note x)
  (set-box! notes (cons x (unbox notes))))
(define (noted)
  (let ([so-far (reverse (unbox notes))])
    (set-box! notes '())
    so-far))
(define (message e)
  (trim (error-object-message e)))

;; Leaving through one builtin. What comes after the escape still runs
;; where it should, as many times as it should.
(define (first-over limit items)
  (call/cc (lambda (k)
             (transduce items (mapping (lambda (x) (note x) (if (> x limit) (k x) x))) (into-list))
             'none)))
(assert! (= (first-over 1 (list 1 2 3)) 2))
(assert! (equal? (noted) '(1 2)))
(assert! (eq? (first-over 5 (list 1 2 3)) 'none))
(assert! (equal? (noted) '(1 2 3)))
(assert! (equal? (list (first-over 1 (list 1 2 3)) (first-over 2 (list 1 2 3))) '(2 3)))
(noted)
(assert! (= (+ 1 (first-over 1 (list 1 2 3)) 10) 13))
(noted)

;; The builtin is fine to use again.
(assert! (equal? (transduce (list 1 2 3) (mapping (lambda (x) (* 2 x))) (into-list)) '(2 4 6)))

;; Often enough to be compiled.
(define (leave-many n acc)
  (if (= n 0) acc (leave-many (- n 1) (+ acc (first-over 1 (list 1 2 3))))))
(assert! (= (leave-many 10000 0) 20000))
(noted)

;; Through two builtins at once.
(define (first-pair)
  (call/cc (lambda (k)
             (transduce (list 1 2)
                        (mapping (lambda (x)
                                   (transduce (list 10 20) (mapping (lambda (y) (k (list x y)))) (into-list))))
                        (into-list)))))
(assert! (equal? (first-pair) '(1 10)))
(assert! (equal? (list (first-pair) (first-pair)) '((1 10) (1 10))))

;; To a continuation that is itself under a builtin.
(define (leave-the-inner-one)
  (transduce (list 1 2 3)
             (mapping (lambda (x)
                        (call/cc (lambda (k)
                                   (transduce (list 10 20) (mapping (lambda (y) (k (+ x y)))) (into-list))))))
             (into-list)))
(assert! (equal? (leave-the-inner-one) '(11 12 13)))

;; A continuation captured and called within one callback.
(define (within-a-callback)
  (transduce (list 1 2 3) (mapping (lambda (x) (+ x (call/cc (lambda (k) (k 10)))))) (into-list)))
(assert! (equal? (within-a-callback) '(11 12 13)))

;; Handlers on the way out are passed over, and are not left installed.
(define (past-handlers)
  (call/cc (lambda (k)
             (with-handler (lambda (e) 'outer-handler)
                           (transduce (list 1 2 3)
                                      (mapping (lambda (x) (with-handler (lambda (e) 'inner-handler) (k 'left))))
                                      (into-list))))))
(assert! (eq? (past-handlers) 'left))
(define (handler-after-leaving)
  (with-handler (lambda (e) 'handled)
                (call/cc (lambda (k)
                           (transduce (list 1 2 3) (mapping (lambda (x) (k 'left))) (into-list))))
                (error "after leaving")))
(assert! (eq? (handler-after-leaving) 'handled))

;; `dynamic-wind` is left on the way.
(define (leave-a-wind)
  (call/cc (lambda (k)
             (transduce (list 1 2 3)
                        (mapping (lambda (x)
                                   (dynamic-wind (lambda () (note 'in))
                                                 (lambda () (if (= x 2) (k 'left) x))
                                                 (lambda () (note 'out)))))
                        (into-list)))))
(assert! (eq? (leave-a-wind) 'left))
(assert! (equal? (noted) '(in out in out)))

;; An error and an escape from the same builtin, one after the other.
(define (error-then-leave)
  (list (with-handler (lambda (e) 'handled)
                      (transduce (list 1 2 3) (mapping (lambda (x) (error "in a callback"))) (into-list)))
        (first-over 1 (list 1 2 3))
        (with-handler (lambda (e) 'handled)
                      (transduce (list 1 2 3) (mapping (lambda (x) (car x))) (into-list)))))
(assert! (equal? (error-then-leave) '(handled 2 handled)))
(noted)

;; One captured in a callback, called after the builtin has returned.
(define kept (box #f))
(define (keep-from-a-callback)
  (transduce (list 1 2 3)
             (mapping (lambda (x)
                        (call/cc (lambda (k)
                                   (when (= x 2)
                                     (set-box! kept k))
                                   x))))
             (into-list)))
(assert! (equal? (keep-from-a-callback) '(1 2 3)))
(define (call-the-kept-one)
  (with-handler (lambda (e) (message e)) ((unbox kept) 20)))
(assert! (equal? (call-the-kept-one)
                 "this continuation was captured under a call from native code that has returned"))
;; And from a later callback of the same builtin.
(define (call-from-the-next-callback)
  (transduce (list 1 2 3)
             (mapping (lambda (x)
                        (if (= x 3)
                            (with-handler (lambda (e) 'refused) ((unbox kept) 20))
                            (call/cc (lambda (k)
                                       (set-box! kept k)
                                       x)))))
             (into-list)))
(assert! (equal? (call-from-the-next-callback) '(1 2 refused)))
;; Everything still works afterwards.
(assert! (= (first-over 1 (list 1 2 3)) 2))
(noted)

;; `apply` takes a continuation, here and through a builtin.
(define (applied)
  (+ 1 (call/cc (lambda (k) (apply k (list 5))))))
(assert! (= (applied) 6))
(define (applied-under-a-builtin)
  (call/cc (lambda (k)
             (transduce (list 1 2 3) (mapping (lambda (x) (apply k (list 'left)))) (into-list)))))
(assert! (eq? (applied-under-a-builtin) 'left))
(define (applied-to-too-many)
  (with-handler (lambda (e) 'refused) (+ 1 (call/cc (lambda (k) (apply k (list 5 6)))))))
(assert! (eq? (applied-to-too-many) 'refused))

;; One kept from an earlier top level form can still be called.
(define from-the-top (box #f))
(define at-the-top (+ 1 (call/cc (lambda (k) (set-box! from-the-top k) 1))))
(when (= at-the-top 2)
  ((unbox from-the-top) 10))
(assert! (= at-the-top 11))
