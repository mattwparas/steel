(define values list)
(define (call-with-values producer consumer)
  (define result (apply consumer (producer)))
  (cond
    [(not (list? result)) result]
    [(= (length result) 1) (car result)]
    [else result]))
; (if (= (length result) 1) (car result) result))

(define (hide r x)
  (call-with-values (lambda () (values (vector values (lambda (x) x)) (if (< r 100) 0 1)))
                    (lambda (v i) ((vector-ref v i) x))))

(define vector->list mutable-vector->list)
(define list-tail drop)

;; Returns a list with n elements, all equal to x.
(define (make-test-list1 n x)
  (if (zero? n) '() (cons x (make-test-list1 (- n 1) x))))

;; Returns a list of n lists, each consisting of n x's.
;; The n elements of the outer list are actually the same list.

(define (make-test-tree1 n)
  (if (zero? n) '() (make-test-list1 n (make-test-tree1 (- n 1)))))

;; Returns a list of n elements, as returned by the thunk.

(define (make-test-list2 n thunk)
  ; (displayln "CALLING make-test-list2" n)
  (if (zero? n) '() (cons (thunk) (make-test-list2 (- n 1) thunk))))

;; Returns a balanced tree of height n, with the branching factor
;; at each level equal to the height of the tree at that level.
;; The subtrees do not share structure.

(define (make-test-tree2 n)
  ; (displayln "CALLING make-test-tree2" n)
  (if (zero? n) '() (make-test-list2 n (lambda () (make-test-tree2 (- n 1))))))

;; Returns an extremely unbalanced tree of height n.

(define (make-test-tree5 n)
  (if (zero? n) '() (cons (make-test-tree5 (- n 1)) 'a)))

;; Calls the thunk n times.

(define (iterate n thunk)
  ; (displayln n)
  (cond
    [(= n 1) (thunk)]
    [(> n 1)
     (thunk)
     (iterate (- n 1) thunk)]
    [else #f]))

;; A simple circular list is a worst case for R5RS equal?.

; (define (equality-benchmark0 n)
;   (let ([x (vector->list (make-vector n 'a))])
;     (set-cdr! (list-tail x (- n 1)) x)
;     (iterate n (hide n (lambda () (equal? x (cdr x)))))))

;; DAG with much sharing.
;; 10 is a good parameter for n.

(define (equality-benchmark1 n)
  (let ([x (make-test-tree1 n)] [y (make-test-tree1 n)])
    (iterate n (hide n (lambda () (equal? x y))))))

;; Tree with no sharing.
;; 8 is a good parameter for n.

(define (equality-benchmark2 n)
  (let ([x (make-test-tree2 n)] [y (make-test-tree2 n)])
    ; (displayln y)

    (iterate n (hide n (lambda () (equal? x y))))))

;; Flat vectors.
;; 1000 might be a good parameter for n.

(define (equality-benchmark3 n)
  (let* ([x (make-vector n 'a)] [y (make-vector n 'a)])
    (iterate n (hide n (lambda () (equal? x y))))))

;; Shallow lists.
;; 300 might be a good parameter for n.

(define (equality-benchmark4 n)
  (let* ([x (vector->list (make-vector n (make-test-tree2 3)))]
         [y (vector->list (make-vector n (make-test-tree2 3)))])
    (iterate n (hide n (lambda () (equal? x y))))))

;; No sharing, no proper lists,
;; and deep following car chains instead of cdr.

(define (equality-benchmark5 n . rest)
  (let* ([x (make-test-tree5 n)] [y (make-test-tree5 n)] [iterations (if (null? rest) n (car rest))])
    (iterate iterations (hide n (lambda () (equal? x y))))))

;; A shorter form of the benchmark above.

(define (equality-benchmark5short n)
  (equality-benchmark5 n 100))

(define (equality-benchmarks n0 n1 n2 n3 n4 n5)
  (and ; (equality-benchmark0 n0) ;; cyclic benchmark does not work on most non-R7RS schemes
   (equality-benchmark1 n1)
   (equality-benchmark2 n2)
   (equality-benchmark3 n3)
   (equality-benchmark4 n4)
   (equality-benchmark5 n5)))

; 100
; 100
; 8
; 1000
; 2000
; 5000
; #t

;; Actual test
; (equality-benchmarks 100 100 8 1000 2000 5000)

;; For test suite

(equality-benchmarks 10 10 3 100 200 500)
