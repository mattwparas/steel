;; A box, vector or struct that has just been made and is waiting to be an
;; argument, while a later argument is worked out by a primitive. Compiled
;; code holds it in a register then. Another thread that collects in the
;; meantime has to find it all the same.
(struct counter (n) #:mutable)

(define stop (box #f))
(define (collect n)
  (if (unbox stop)
      n
      (begin
        (#%gc-collect)
        (collect (+ n 1)))))
(define collector (spawn-native-thread (lambda () (collect 0))))

(define (with-vector n)
  (list (mutable-vector n n) (hash 'n n)))
(define (with-box n)
  (cons (box n) (number->string n)))
(define (with-struct n)
  (list (counter n) (hash 'n n)))
;; Inside something that is not itself collected.
(define (with-a-list-of-one n)
  (list (list (box n)) (hash 'n n)))
;; Several waiting at once, the first one longest.
(define (with-three n)
  (list (box n) (mutable-vector n) (counter n) (hash 'n n)))
;; One waiting in each frame of a recursion.
(define (chain n)
  (if (= n 0)
      '()
      (cons (box n)
            (begin
              (hash 'n n)
              (chain (- n 1))))))
(define (chain-whole? boxes n)
  (or (null? boxes) (and (equal? (unbox (car boxes)) n) (chain-whole? (cdr boxes) (- n 1)))))

(define (whole? n)
  (and (= (vector-length (car (with-vector n))) 2)
       (equal? (unbox (car (with-box n))) n)
       (equal? (counter-n (car (with-struct n))) n)
       (equal? (unbox (car (car (with-a-list-of-one n)))) n)
       (let ([three (with-three n)])
         (and (equal? (unbox (car three)) n)
              (= (vector-length (cadr three)) 1)
              (equal? (counter-n (caddr three)) n)))
       (chain-whole? (chain 5) 5)))

(define (count-broken from to broken)
  (if (= from to) broken (count-broken (+ from 1) to (if (whole? from) broken (+ broken 1)))))

(define trips 6000)
(define (work k)
  (with-handler (lambda (e) 'raised) (count-broken (* k 1000000) (+ (* k 1000000) trips) 0)))
(define workers
  (list (spawn-native-thread (lambda () (work 1)))
        (spawn-native-thread (lambda () (work 2)))
        (spawn-native-thread (lambda () (work 3)))))
(define here (work 0))
(define elsewhere (map thread-join! workers))
(set-box! stop #t)
(assert! (>= (thread-join! collector) 0))
(assert! (equal? here 0))
(assert! (equal? elsewhere '(0 0 0)))
