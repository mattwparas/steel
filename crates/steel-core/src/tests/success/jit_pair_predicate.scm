;; `pair?` on a local, and `car` / `cdr` in the arm where it held.
(define (first-or x default)
  (if (pair? x) (car x) default))

(assert! (equal? (first-or (list 1 2) 'none) 1))
(assert! (equal? (first-or (cons 3 4) 'none) 3))
(assert! (equal? (first-or '() 'none) 'none))
(assert! (equal? (first-or 5 'none) 'none))
(assert! (equal? (first-or "str" 'none) 'none))
(assert! (equal? (first-or (vector 1) 'none) 'none))

;; The false arm learns nothing.
(define (describe x)
  (if (pair? x) (list 'pair (car x) (cdr x)) (list 'other x)))

(assert! (equal? (describe (list 1 2 3)) '(pair 1 (2 3))))
(assert! (equal? (describe (cons 'a 'b)) '(pair a b)))
(assert! (equal? (describe '()) '(other ())))

;; An assignment in the arm forgets what the test proved.
(define (reassigned x)
  (if (pair? x)
      (begin
        (set! x (list 'fresh))
        (car x))
      'none))

(assert! (equal? (reassigned (cons 1 2)) 'fresh))

;; Walking a list, where the tested variable changes every iteration.
(define (count-pairs x n)
  (if (pair? x) (count-pairs (cdr x) (+ n 1)) n))

(assert! (equal? (count-pairs (list 1 2 3 4 5) 0) 5))
(assert! (equal? (count-pairs (cons 1 (cons 2 3)) 0) 2))
(assert! (equal? (count-pairs '() 0) 0))

(define (sum-cars x acc)
  (if (pair? x) (sum-cars (cdr x) (+ acc (car x))) acc))

(assert! (equal? (sum-cars (list 1 2 3 4) 0) 10))

;; In tail position, the result itself.
(define (is-pair x) (pair? x))
(assert! (equal? (map is-pair (list (list 1) '() (cons 1 2) 1 "a")) '(#t #f #t #f #f)))
