;; Captures are read when used rather than when pushed, and a consumer that
;; only reads one skips the clone: `set!` on a captured variable, and locals or
;; captures passed to a primitive. Run hot so the jitted versions are checked.
(define (bump x)
  (+ x 1))

(define (make-counter)
  (define n 0)
  (define total 0)
  (define label "count")
  (lambda (k)
    (set! n (+ n 1))
    ;; A call between reading the box and writing it.
    (set! total (bump (+ total k)))
    ;; A capture across a branch, and passed to primitives.
    (if (even? n)
        (string-append label ":" (number->string n))
        (list (string-length label) total))))

(define counter (make-counter))
(define (drive i last)
  (if (= i 0)
      last
      (drive (- i 1) (counter i))))
(assert! (equal? (drive 4001 #f) (list 5 (+ 4001 (/ (* 4001 4002) 2)))))
(assert! (equal? (counter 0) "count:4002"))

;; A primitive that raises on a captured argument.
(define (make-bad s)
  (lambda ()
    (with-handler (lambda (e) 'raised) (string-length s))))
(define bad (make-bad 12))
(define (loop-bad n)
  (if (= n 0)
      (bad)
      (begin
        (bad)
        (loop-bad (- n 1)))))
(assert! (equal? (loop-bad 2000) 'raised))

;; A captured value returned, stored, and compared after the frame that read it
;; is gone.
(define (make-holder v)
  (lambda () (list v v)))
(define holders (map (lambda (i) (make-holder (string-append "v" (number->string i)))) (range 0 50)))
(define (collect hs acc)
  (if (null? hs)
      acc
      (collect (cdr hs) (cons ((car hs)) acc))))
(define (loop-collect n acc)
  (if (= n 0)
      acc
      (loop-collect (- n 1) (collect holders '()))))
(define collected (loop-collect 200 '()))
(assert! (equal? (car collected) '("v49" "v49")))
(assert! (equal? (length collected) 50))
