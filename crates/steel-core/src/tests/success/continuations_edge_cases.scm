;; The less usual ways to make, pass and call a continuation.

;; The receiver can be a primitive, or another continuation.
(define (receiver-is-a-primitive)
  (let ([k (call/cc (lambda (k) k))])
    (if (continuation? k) (k 'called) k)))
(assert! (eq? (receiver-is-a-primitive) 'called))

(define (receiver-is-a-continuation)
  (let ([outer (call/cc (lambda (out) (call/cc out)))])
    ;; `out` was handed the inner continuation, so that is what comes back.
    (if (continuation? outer) (outer 'second) outer)))
(assert! (eq? (receiver-is-a-continuation) 'second))

;; Not calling the continuation at all.
(assert! (= (call/cc (lambda (k) 5)) 5))
(assert! (= (+ 1 (call/cc (lambda (k) (+ 10 (k 1))))) 2))
(assert! (continuation? (call/cc (lambda (k) k))))
(assert! (not (continuation? (lambda (x) x))))
(assert! (not (continuation? 5)))

;; Both spellings are the same thing.
(assert! (= (call-with-current-continuation (lambda (k) (k 7))) 7))

;; Called from inside functions that take a function: the escape has to get
;; out of those as well.
(define (first-negative items)
  (call/cc (lambda (return)
             (for-each (lambda (x) (when (< x 0) (return x))) items)
             'none)))
(assert! (= (first-negative '(3 1 -4 1 -5)) -4))
(assert! (eq? (first-negative '(3 1 4)) 'none))

(define (product items)
  (call/cc (lambda (return)
             (let loop ([rest items])
               (cond
                 [(null? rest) 1]
                 [(= 0 (car rest)) (return 0)]
                 [else (* (car rest) (loop (cdr rest)))])))))
(assert! (= (product '(1 2 3 4)) 24))
(assert! (= (product '(1 2 0 4)) 0))

(define (find-in-map items)
  (call/cc (lambda (return) (map (lambda (x) (if (eq? x 'stop) (return 'stopped) x)) items))))
(assert! (equal? (find-in-map '(a b c)) '(a b c)))
(assert! (eq? (find-in-map '(a stop c)) 'stopped))

;; Escaping out of an error handler, and out of the body it guards.
(define (escape-from-handler)
  (call/cc (lambda (k) (with-handler (lambda (e) (k 'from-handler)) (error "boom")))))
(assert! (eq? (escape-from-handler) 'from-handler))

(define (escape-past-handler)
  (let ([handled (box #f)])
    (list (call/cc (lambda (k) (with-handler (lambda (e) (set-box! handled #t)) (k 'escaped))))
          (unbox handled))))
(assert! (equal? (escape-past-handler) '(escaped #f)))

;; An error after an escape is still caught by the handler outside.
(define (error-after-escape)
  (with-handler (lambda (e) 'caught)
                (begin
                  (call/cc (lambda (k) (k 1)))
                  (error "later"))))
(assert! (eq? (error-after-escape) 'caught))

;; Calling a continuation with the wrong number of arguments, or `call/cc`
;; with something that is not a function, is an error and not a crash.
(define (raises? thunk) (eq? 'raised (with-handler (lambda (e) 'raised) (thunk))))
(assert! (raises? (lambda () (call/cc 5))))
(assert! (raises? (lambda () (call/cc (lambda () 1)))))
(assert! (raises? (lambda () (call/cc (lambda (a b) 1)))))

;; In a loop that runs long enough to be compiled, both escaping and not.
(define (hot-escapes n)
  (let loop ([i 0] [acc 0])
    (if (= i n)
        acc
        (loop (+ i 1)
              (+ acc (call/cc (lambda (k) (if (even? i) (k 1) 2))))))))
(assert! (= (hot-escapes 30000) 45000))

;; Deep recursion that escapes from the bottom, many times over.
(define (dive d k) (if (= d 0) (k 'bottom) (cons d (dive (- d 1) k))))
(define (dive-many n)
  (let loop ([i 0] [ok #t])
    (if (= i n) ok (loop (+ i 1) (and ok (eq? 'bottom (call/cc (lambda (k) (dive 500 k)))))))))
(assert! (dive-many 300))

;; A continuation in tail position, and one called in tail position.
(define (tail-capture n) (if (= n 0) 'done (call/cc (lambda (k) (tail-capture (- n 1))))))
(assert! (eq? (tail-capture 20000) 'done))
(define (tail-call-of-k n k) (if (= n 0) (k 'reached) (tail-call-of-k (- n 1) k)))
(assert! (eq? (call/cc (lambda (k) (tail-call-of-k 20000 k))) 'reached))

;; Passed through many hands on the way up before it is called. Each hand
;; holds it for a while, and none keeps it.
(define (hand-on k depth)
  (let ([held (list k k)]
        [also (lambda () k)])
    (if (= depth 0)
        ((car held) 'handed-on)
        (+ 1 (hand-on ((lambda () (also))) (- depth 1))))))
(assert! (eq? (call/cc (lambda (k) (hand-on k 100))) 'handed-on))

;; Held by a great many things on the stack at once, none of which outlives
;; it. More than the stack is looked through for, so this one is copied, and
;; still has to work.
(define (held-by-many k n holders)
  (if (= n 0)
      ((car holders) 'still-works)
      (held-by-many k (- n 1) (cons (lambda (x) (k x)) holders))))
(assert! (eq? (call/cc (lambda (k) (held-by-many k 200 '()))) 'still-works))

;; Kept by one of the things it was passed to, and gone back into after the
;; rest have let go.
(define (keep-one)
  (let* ([shelf (box #f)]
         [times (box 0)]
         [v (call/cc (lambda (k)
                       (let ([holders (list (lambda (x) (k x)) (lambda (x) (k x)) (lambda (x) (k x)))])
                         (set-box! shelf (cadr holders))
                         0)))])
    (set-box! times (+ (unbox times) 1))
    (if (< v 3)
        ((unbox shelf) (+ v 1))
        (list v (unbox times)))))
(assert! (equal? (keep-one) '(3 4)))

;; Captured on one thread and used there, while another thread collects.
(define (escapes-while-collecting)
  (let* ([stop (box #f)]
         [collector (spawn-native-thread (lambda ()
                                           (let loop ([n 0])
                                             (if (unbox stop)
                                                 n
                                                 (begin
                                                   (#%gc-collect)
                                                   (loop (+ n 1)))))))]
         [total (let loop ([i 0] [acc 0])
                  (if (= i 20000)
                      acc
                      (loop (+ i 1) (+ acc (call/cc (lambda (k) (k (car (list 1 (box i))))))))))])
    (set-box! stop #t)
    (thread-join! collector)
    total))
(assert! (= (escapes-while-collecting) 20000))
