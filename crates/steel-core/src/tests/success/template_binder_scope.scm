;; A name a template binds is renamed only where the binding is in scope. The
;; same name outside it is free, and means the global.

(define t 100)
(define-syntax pair-with-global (syntax-rules () [(_ e) (list (let ([t 1]) e) t)]))
(assert! (equal? (pair-with-global (pair-with-global 0)) '((0 100) 100)))

(define-syntax gen
  (syntax-rules ()
    [(_ name) (define-syntax name (syntax-rules () [(_ e) (list (let ([t 1]) e) t)]))]))
(gen generated)
(assert! (equal? (generated (generated 0)) '((0 100) 100)))

;; A let's values are outside its bindings
(define-syntax rebind (syntax-rules () [(_ e) (let ([t (+ t 1)]) (list t e))]))
(assert! (equal? (rebind t) '(101 100)))

;; The defines of a body are in scope before them too
(define-syntax evens
  (syntax-rules ()
    [(_ n)
     (let ()
       (define (ev? k) (if (= k 0) #t (od? (- k 1))))
       (define (od? k) (if (= k 0) #f (ev? (- k 1))))
       (ev? n))]))
(assert! (equal? (list (evens 10) (evens 7)) '(#t #f)))
(define (in-a-function) (evens 4))
(assert! (in-a-function))

(define-syntax count-up
  (syntax-rules ()
    [(_ n) (let loop ([i 0] [acc '()]) (if (= i n) (reverse acc) (loop (+ i 1) (cons i acc))))]))
(assert! (equal? (count-up 3) '(0 1 2)))

(define loop 'outer-loop)
(define-syntax mixed (syntax-rules () [(_ e) (list ((lambda (t) (* t 2)) 5) t e loop)]))
(assert! (equal? (mixed t) '(10 100 100 outer-loop)))

(define-syntax body-define (syntax-rules () [(_ e) ((lambda () (define t 7) (list t e)))]))
(assert! (equal? (body-define t) '(7 100)))

(define-syntax top-defines
  (syntax-rules () [(_ get) (begin (define (get) (helper)) (define (helper) 'helped))]))
(top-defines gimme)
(assert! (eq? (gimme) 'helped))
