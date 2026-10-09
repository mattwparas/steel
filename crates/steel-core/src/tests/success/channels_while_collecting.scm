;; A value in a channel is held by neither thread. It has to last through a
;; collection all the same.
(struct pair-of (left right) #:mutable)

(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))

(define channels (channels/new))
(define sender (channels-sender channels))
(define receiver (channels-receiver channels))

(channel/send sender (box 42))
(channel/send sender (mutable-vector 1 2 3))
(channel/send sender (pair-of (box 1) (list 2)))
(collect 3)
(assert! (= (unbox (channel/recv receiver)) 42))
(assert! (equal? (vector->list (channel/recv receiver)) '(1 2 3)))
(define received (channel/recv receiver))
(collect 3)
(assert! (= (unbox (pair-of-left received)) 1))
(assert! (equal? (pair-of-right received) '(2)))

;; One thread sends while this one receives and a third collects.
(define count 20000)
(define (send n)
  (when (> n 0)
    (channel/send sender (box (list n n)))
    (send (- n 1))))
(define sending (spawn-native-thread (lambda () (send count))))
(define collecting (spawn-native-thread (lambda () (collect 200))))
(define (receive n)
  (when (> n 0)
    (assert! (equal? (unbox (channel/recv receiver)) (list n n)))
    (receive (- n 1))))
(receive count)
(thread-join! sending)
(thread-join! collecting)
