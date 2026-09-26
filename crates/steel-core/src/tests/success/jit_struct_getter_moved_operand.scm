;; A mutable struct's accessor reads through the two argument getter prototype.
;; When the struct is the last use of a local, the read moves it out of its
;; register, and a call that doesn't match the descriptor falls back to the
;; real getter. That call has to see the struct, not the emptied register.

(struct foo (a) #:mutable)
(struct bar (a) #:mutable)

(define (get x) (foo-a x))

(define (warm n)
  (if (= n 0)
      0
      (begin
        (get (foo n))
        (warm (- n 1)))))

(warm 1000)

(define (message-of thunk)
  (with-handler (lambda (e) (error-object-message e)) (thunk)))

;; the wrong struct
(assert! (string-contains? (message-of (lambda () (get (bar 1)))) "bar"))

;; not a struct at all
(assert! (string-contains? (message-of (lambda () (get 10))) "10"))

(assert! (equal? (get (foo 7)) 7))
