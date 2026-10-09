;; A thread waiting in a join does not hold up what other threads do to the
;; one it waits for, and does not hold up collections.
(require-builtin steel/time)

(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))
(define (allocate i)
  (box i)
  (allocate (+ i 1)))
(define (raised? thread)
  (eq? 'raised (with-handler (lambda (e) 'raised) (thread-join! thread))))

;; Interrupted while joined on.
(define endless (spawn-native-thread (lambda () (allocate 0))))
(define interrupting
  (spawn-native-thread (lambda ()
                         (time/sleep-ms 50)
                         (thread-interrupt endless))))
(assert! (raised? endless))
(thread-join! interrupting)

;; Collecting and resumed while joined on.
(define collecting (spawn-native-thread (lambda () (collect 500) 'done)))
(define resuming
  (spawn-native-thread (lambda ()
                         (time/sleep-ms 20)
                         (thread-resume collecting))))
(assert! (eq? (thread-join! collecting) 'done))
(thread-join! resuming)

;; What a thread returns is held by nothing but the thread until it is
;; joined, and has to last through the collections in between.
(struct pair-of (left right) #:mutable)
(define returning
  (spawn-native-thread (lambda () (list (box 42) (mutable-vector 1 2 3) (pair-of 1 (list 2))))))
(time/sleep-ms 50)
(collect 3)
(define returned (thread-join! returning))
(collect 3)
(assert! (= (unbox (car returned)) 42))
(assert! (equal? (vector->list (cadr returned)) '(1 2 3)))
(assert! (equal? (pair-of-right (caddr returned)) '(2)))

;; Many short threads, each leaving cells behind.
(struct small (a b c d) #:mutable)
(define (churn n)
  (when (> n 0)
    (small n n n n)
    (churn (- n 1))))
(define (come-and-go n)
  (when (> n 0)
    (thread-join! (spawn-native-thread (lambda () (churn 500))))
    (when (= 0 (modulo n 50))
      (#%gc-collect))
    (come-and-go (- n 1))))
(come-and-go 300)
