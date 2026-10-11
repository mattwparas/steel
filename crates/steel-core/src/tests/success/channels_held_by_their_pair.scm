;; A program can hold nothing of a channel but the pair it came in, and ask
;; for an end each time. What is waiting in the channel is still in use.

(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))

(define channels (channels/new))

(define (send-all n)
  (when (> n 0)
    (channel/send (channels-sender channels) (box (list n n)))
    (send-all (- n 1))))

(send-all 1000)
(collect 5)

;; So that slots freed by mistake are filled with something else
(define other-boxes (map (lambda (i) (box i)) (range 0 5000)))

(define (receive-all n)
  (when (> n 0)
    (assert! (equal? (unbox (channel/recv (channels-receiver channels))) (list n n)))
    (receive-all (- n 1))))

(receive-all 1000)
(assert! (= (length other-boxes) 5000))

;; The same value sent twice, and lists that share their storage, are each
;; kept until they are received.
(define shared (list (box 'a) (box 'b) (box 'c)))
(define (send-shared)
  (channel/send (channels-sender channels) shared)
  (channel/send (channels-sender channels) (cdr shared))
  (channel/send (channels-sender channels) shared))
(send-shared)
(set! shared #f)
(collect 2)

(define tail-first
  (let* ([whole (channel/recv (channels-receiver channels))]
         [tail (channel/recv (channels-receiver channels))])
    (collect 2)
    (list (map unbox whole) (map unbox tail))))
(assert! (equal? tail-first '((a b c) (b c))))
(collect 2)
(assert! (equal? (map unbox (channel/recv (channels-receiver channels))) '(a b c)))
