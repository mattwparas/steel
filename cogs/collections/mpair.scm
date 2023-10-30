;; TODO: Implement mutable pairs
(struct mcons (mcar mcdr) #:mutable)

(define set-car! set-mcons-mcar!)
(define set-cdr! set-mcons-mcdr!)

;; Mutable cons!
(define (mcons->list mutable-cons)

  (define (loop mutable-cons builder)
    (if (not (mcons? (mcons-mcdr mutable-cons)))

        (cons (mcons-mcar mutable-cons) builder)

        (loop (mcons-mcdr mutable-cons) (cons (mcons-mcar mutable-cons) builder))))

  (reverse (loop mutable-cons '())))

;; Can make a loop, and garbage collection solves it!
(define (loop)

  (define my-cons (mcons 10 (mcons 20 (mcons 30 void))))

  ;; Make a cycle!
  (set-car! my-cons my-cons)

  (loop))
