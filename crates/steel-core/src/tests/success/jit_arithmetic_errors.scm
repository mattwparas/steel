;; Errors raised out of jitted arithmetic have to come back as steel errors the
;; handler can catch. The two argument helpers used to unwrap the Result, which
;; took the whole process down on something as ordinary as a divide by zero.

(define (div a b) (/ a b))
(define (mul a b) (* a b))
(define (sub a b) (- a b))

(assert! (equal? (with-handler (lambda (e) 'caught) (div 10 0)) 'caught))
(assert! (equal? (with-handler (lambda (e) 'caught) (div 10 "s")) 'caught))
(assert! (equal? (with-handler (lambda (e) 'caught) (mul 10 "s")) 'caught))
(assert! (equal? (with-handler (lambda (e) 'caught) (mul "s" 10)) 'caught))
(assert! (equal? (with-handler (lambda (e) 'caught) (sub 10 "s")) 'caught))

;; and the non error cases still work
(assert! (equal? (div 10 2) 5))
(assert! (equal? (mul 10 2) 20))
(assert! (equal? (sub 10 2) 8))

;; = went further wrong: the helper set the error but nothing checked for it
;; until after the jit had popped the frame holding the handler, so the error
;; escaped with-handler entirely
(define (num-eq a b) (= a b))

(assert! (equal? (with-handler (lambda (e) 'caught) (num-eq 10 "s")) 'caught))
(assert! (equal? (with-handler (lambda (e) 'caught) (num-eq "s" 10)) 'caught))
(assert! (equal? (with-handler (lambda (e) 'caught) (= 10 "s")) 'caught))
(assert! (equal? (with-handler (lambda (e) 'caught) (= 'a 1)) 'caught))
(assert! (equal? (num-eq 10 10) #true))
(assert! (equal? (num-eq 10 11) #false))

;; the handler still gets the value it returns, not just a marker
(assert! (equal? (with-handler (lambda (e) 99) (num-eq 1 "s")) 99))

;; through an indirect call too, so the jit can't see the callee
(define (via f a b) (f a b))
(assert! (equal? (with-handler (lambda (e) 'caught) (via div 1 0)) 'caught))
(assert! (equal? (via div 10 5) 2))
(assert! (equal? (with-handler (lambda (e) 'caught) (via num-eq 1 "s")) 'caught))
