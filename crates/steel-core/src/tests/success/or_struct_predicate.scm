;; `(or (p? x) y)` with a struct predicate drops the binding `or` expands into.
;; The result is still the predicate's boolean, or `y`.
(struct point (x y))
(struct cell (value) #:mutable)

(define (point-or-number x)
  (or (point? x) (number? x)))

(define (cell-or x fallback)
  (or (cell? x) fallback))

(define (first-of x)
  (if (or (cell? x) (pair? x))
      (if (cell? x) (cell-value x) (car x))
      'neither))

(define (loop n acc)
  (if (= n 0)
      acc
      (loop (- n 1)
            (list (point-or-number (point 1 2))
                  (point-or-number 5)
                  (point-or-number "no")
                  (cell-or (cell 1) 'fallback)
                  (cell-or 1 'fallback)
                  (first-of (cell 'in-cell))
                  (first-of '(in-list))
                  (first-of 7)))))

(assert! (equal? (loop 3000 '()) '(#t #t #f #t fallback in-cell in-list neither)))

;; A predicate name that is assigned is not known to be boolean valued.
(struct thing (a))
(define other? thing?)
(set! other? (lambda (x) (if (thing? x) 'yes #f)))
(define (check x)
  (or (other? x) 'no))
(assert! (equal? (check (thing 1)) 'yes))
(assert! (equal? (check 1) 'no))
