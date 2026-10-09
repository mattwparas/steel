;; A weak box doesn't keep a registered value from being ready. The box
;; still reads the value until its will has run and let go of it.
(struct frozen (payload label))

(define executor (make-will-executor))
(define ran (box 0))
(define read-by-will (box #f))
(define weak (box #f))
(define other-weak (box #f))
(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))

(define (register make)
  (let ([v (make)])
    (set-box! weak (make-weak-box v))
    (set-box! other-weak (make-weak-box v))
    (will-register executor
                   v
                   (lambda (handed)
                     (set-box! ran (+ 1 (unbox ran)))
                     (set-box! read-by-will (equal? (weak-box-value (unbox weak)) handed)))))
  #t)

(define (readable?)
  (if (weak-box-value (unbox weak)) #t #f))

(define (check make)
  (set-box! ran 0)
  (set-box! read-by-will #f)
  (register make)
  (collect 6)
  ;; Ready, and still there to read.
  (assert! (readable?))
  (will-execute executor)
  (assert! (= (unbox ran) 1))
  (assert! (unbox read-by-will))
  (collect 6)
  (assert! (not (weak-box-value (unbox weak))))
  (assert! (not (weak-box-value (unbox other-weak))))
  #t)

(check (lambda () (list 1 2 3)))
(check (lambda () (frozen 1 'struct)))
(check (lambda () (string-append "a" "b")))
(check (lambda () (hash 'a 1)))
(check (lambda () (box 1)))
(check (lambda () (mutable-vector 1 2)))
(check (lambda ()
         (let* ([cell (box #f)]
                [s (frozen cell 'cycle)])
           (set-box! cell s)
           s)))

;; Two weak boxes with the one value, and nothing else. Neither keeps it for
;; the other.
(define (box-twice make)
  (let ([v (make)])
    (set-box! weak (make-weak-box v))
    (set-box! other-weak (make-weak-box v)))
  #t)
(define (both-empty make)
  (box-twice make)
  (collect 6)
  (assert! (not (weak-box-value (unbox weak))))
  (assert! (not (weak-box-value (unbox other-weak))))
  #t)
(both-empty (lambda () (list 1 2 3)))
(both-empty (lambda () (frozen 1 'struct)))
(both-empty (lambda () (box 1)))

;; A value something still holds stays in both.
(define held (list 1 2 3))
(set-box! weak (make-weak-box held))
(set-box! other-weak (make-weak-box held))
(collect 6)
(assert! (equal? (weak-box-value (unbox weak)) '(1 2 3)))
(assert! (equal? (weak-box-value (unbox other-weak)) '(1 2 3)))
