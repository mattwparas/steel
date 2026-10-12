(define slot (make-tls 0))
(define other (make-tls 'other))

(define (count-up n)
  (when (> n 0)
    (set-tls! slot (+ 1 (get-tls slot)))
    (count-up (- n 1))))
(count-up 5000)
(assert! (= 5000 (get-tls slot)))
(assert! (eq? 'other (get-tls other)))

(define (sum-of handle n acc)
  (if (= n 0) acc (sum-of handle (- n 1) (+ acc (get-tls handle)))))
(assert! (= 25000000 (sum-of slot 5000 0)))

;; The old value is released when a slot is replaced
(define (fresh-lists n)
  (when (> n 0)
    (set-tls! other (list n n n))
    (fresh-lists (- n 1))))
(fresh-lists 5000)
(assert! (equal? (list 1 1 1) (get-tls other)))
(define kept (get-tls other))
(set-tls! other 'other)
(assert! (equal? (list 1 1 1) kept))

;; A thread starts with its spawner's values and writes its own
(define (in-thread)
  (let ([before (get-tls slot)])
    (count-up 3000)
    (list before (get-tls slot) (get-tls other))))
(define threads
  (list (spawn-native-thread in-thread) (spawn-native-thread in-thread)))
(count-up 100)
(for-each (lambda (thread) (assert! (equal? (list 5000 8000 'other) (thread-join! thread))))
          threads)
(assert! (= 5100 (get-tls slot)))

;; A thread doesn't have slots made after it was spawned
(define ready (box #f))
(define (wait-for-reader)
  (if (unbox ready) (unbox ready) (wait-for-reader)))
(define waiting
  (spawn-native-thread (lambda ()
                         (let ([read (wait-for-reader)])
                           (list (with-handler (lambda (e) 'none) (read 2000 0))
                                 (get-tls slot))))))
(define late (make-tls 7))
(define (read-late n acc)
  (if (= n 0) acc (read-late (- n 1) (+ acc (get-tls late)))))
(define (write-late n)
  (when (> n 0)
    (set-tls! late n)
    (write-late (- n 1))))
(assert! (= 14000 (read-late 2000 0)))
(write-late 2000)
(assert! (= 1 (get-tls late)))
(set-box! ready read-late)
(assert! (equal? (list 'none 5100) (thread-join! waiting)))

(define writing
  (spawn-native-thread (lambda () (list (get-tls late) (begin (write-late 500) (get-tls late))))))
(assert! (equal? (list 1 1) (thread-join! writing)))

;; Not a handle
(assert! (eq? 'refused (with-handler (lambda (e) 'refused) (get-tls 5))))
(assert! (eq? 'refused (with-handler (lambda (e) 'refused) (set-tls! 'slot 5))))
