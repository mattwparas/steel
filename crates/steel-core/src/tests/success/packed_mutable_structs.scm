;; Mutable structs keep their fields inline, in one allocation the collector
;; owns. Run hot so the jitted accessors are the ones checked.
(struct mpair (mcar mcdr) #:mutable #:transparent)
(struct cell (value) #:mutable)
(struct applicable (f extra) #:mutable #:prop:procedure 0)

(define p (mpair 1 2))
(assert! (mpair? p))
(assert! (not (mpair? 5)))
(assert! (not (cell? p)))
(assert! (equal? (mpair-mcar p) 1))
(assert! (equal? (mpair-mcdr p) 2))

;; A setter hands back what the field held.
(assert! (equal? (set-mpair-mcar! p 10) 1))
(assert! (equal? (mpair-mcar p) 10))

;; Equality is by contents, identity by `eq?`.
(assert! (equal? (mpair 1 2) (mpair 1 2)))
(assert! (not (equal? (mpair 1 2) (mpair 1 3))))
(assert! (not (equal? (mpair 1 2) (cell 1))))
(assert! (eq? p p))
(assert! (not (eq? (mpair 1 2) (mpair 1 2))))

;; The fields come back as values.
(assert! (equal? (struct->list p) '(10 2)))
(assert! (equal? (struct->list (cell 1)) #f))

;; An applicable struct runs the procedure in its field.
(define add-one (applicable (lambda (x) (+ x 1)) 'ignored))
(assert! (procedure? add-one))
(assert! (equal? (add-one 41) 42))

;; A copy has its own fields.
(define copied (#%struct-update p 'mcar 99))
(assert! (equal? (mpair-mcar copied) 99))
(assert! (equal? (mpair-mcar p) 10))
(set-mpair-mcdr! copied 'changed)
(assert! (equal? (mpair-mcdr p) 2))

;; The wrong type is an error, not a read of whatever is there.
(assert! (equal? (with-handler (lambda (e) 'caught) (mpair-mcar (cell 1))) 'caught))
(assert! (equal? (with-handler (lambda (e) 'caught) (mpair-mcar 5)) 'caught))
(assert! (equal? (with-handler (lambda (e) 'caught) (set-mpair-mcar! '(1 2) 5)) 'caught))

;; A struct can point at itself.
(define self (mpair 1 '()))
(set-mpair-mcdr! self self)
(assert! (eq? (mpair-mcdr self) self))
(assert! (eq? (mpair-mcdr (mpair-mcdr self)) self))

;; Rings are garbage only the collector can find. Enough of them to make it
;; run many times.
(define (make-ring n)
  (define head (mpair 0 '()))
  (let loop ([i 1] [prev head])
    (if (= i n)
        (begin
          (set-mpair-mcdr! prev head)
          head)
        (let ([next (mpair i '())])
          (set-mpair-mcdr! prev next)
          (loop (+ i 1) next)))))

(define (ring-sum r n)
  (let loop ([i 0] [r r] [acc 0])
    (if (= i n) acc (loop (+ i 1) (mpair-mcdr r) (+ acc (mpair-mcar r))))))

(define (churn-rings k acc)
  (if (= k 0) acc (churn-rings (- k 1) (+ acc (ring-sum (make-ring 100) 100)))))

(assert! (equal? (churn-rings 5000 0) 24750000))

;; Chains are garbage the last handle finds. A long one must come apart
;; without recursing a frame per link.
(define (build n acc)
  (if (= n 0) acc (build (- n 1) (mpair n acc))))

(define (chain-length l n)
  (if (null? l) n (chain-length (mpair-mcdr l) (+ n 1))))

(define (churn-chains k acc)
  (if (= k 0) acc (churn-chains (- k 1) (+ acc (chain-length (build 1000 '()) 0)))))

(assert! (equal? (churn-chains 500 0) 500000))
(assert! (equal? (chain-length (build 500000 '()) 0) 500000))

;; A struct type made inside a function has no fixed key.
(define (make-counter)
  (struct counter (n) #:mutable)
  (define c (counter 0))
  (let loop ([i 0])
    (when (< i 1000)
      (set-counter-n! c (+ (counter-n c) 1))
      (loop (+ i 1))))
  (counter-n c))

(assert! (equal? (make-counter) 1000))
(assert! (equal? (make-counter) 1000))

;; A field holding a struct of another type, and fields read through locals
;; that a loop keeps for its whole run.
(define (sum-cells cells total)
  (if (null? cells) total (sum-cells (cdr cells) (+ total (cell-value (car cells))))))

(define cells (map cell (range 0 100)))
(define (bump-all cells)
  (for-each (lambda (c) (set-cell-value! c (+ (cell-value c) 1))) cells))

(let loop ([i 0])
  (when (< i 2000)
    (bump-all cells)
    (loop (+ i 1))))

(assert! (equal? (sum-cells cells 0) (+ 4950 (* 100 2000))))

;; A struct is a hash key by its contents.
(define table (hash (mpair 1 2) 'found))
(assert! (equal? (hash-ref table (mpair 1 2)) 'found))

;; Printing one that points at itself ends.
(assert! (string? (to-string self)))

;; A field read that goes straight into a vector operation, in a loop.
(struct holder (items count) #:mutable)
(define h (holder (vector 1 2 3 4) 4))
(define (sum-holder h)
  (let loop ([i 0] [total 0])
    (if (= i (holder-count h)) total (loop (+ i 1) (+ total (vector-ref (holder-items h) i))))))
(define (repeat-sum k acc)
  (if (= k 0) acc (repeat-sum (- k 1) (+ acc (sum-holder h)))))
(assert! (equal? (repeat-sum 5000 0) 50000))

;; A stored value that takes a call to compute.
(define (next-value c) (+ (cell-value c) 1))
(define (count-up c n)
  (if (= n 0)
      (cell-value c)
      (begin
        (set-cell-value! c (next-value c))
        (count-up c (- n 1)))))
(assert! (equal? (count-up (cell 0) 5000) 5000))

;; Two threads on one struct. Every value either of them stores is a three
;; element list, so every read has to see one whole.
(define shared (cell (list 0 0 0)))
(define (writer n)
  (when (> n 0)
    (set-cell-value! shared (list n n n))
    (writer (- n 1))))
(define (reader n ok)
  (if (= n 0)
      ok
      (let ([v (cell-value shared)])
        (reader (- n 1) (and ok (list? v) (= (length v) 3) (equal? (car v) (caddr v)))))))
(define other (spawn-native-thread (lambda () (writer 20000))))
(assert! (reader 20000 #t))
(writer 2000)
(thread-join! other)
(assert! (reader 100 #t))
