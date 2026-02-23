(define-syntax reset
  (syntax-rules ()
    [(reset ?e) (*reset (lambda () ?e))]))

(define-syntax shift
  (syntax-rules ()
    [(shift ?k ?e) (*shift (lambda (?k) ?e))]))

(define *meta-continuation* (make-tls (lambda (_) (error "You forgot the top-level reset..."))))

(define (*abort thunk)
  (let ([v (thunk)]) ((get-tls *meta-continuation*) v)))

(define (*reset thunk)
  (let ([mc (get-tls *meta-continuation*)])
    (call/cc (lambda (k)
               (begin

                 (set-tls! *meta-continuation*
                           (lambda (v)
                             (set-tls! *meta-continuation* mc)
                             (k v)))

                 (*abort thunk))))))

(define (*shift f)
  (call/cc (lambda (k) (*abort (lambda () (f (lambda (v) (reset (k v)))))))))

(define (void)
  (#%void))

(assert! (equal? (* 2 (reset (+ 1 (shift k (k 5))))) 12))

(assert! (equal? (* 2 (reset (+ 1 (shift k 5)))) 10))

(assert! (equal? (+ 1 (reset (* 2 (shift k (k (k 4)))))) 17))

(assert! (equal? (reset (begin
                          (shift k (cons 1 (k (void)))) ;; (1)
                          '()))
                 '(1)))

(assert! (equal? (reset (begin
                          (shift k (cons 1 (k (void))))
                          (shift k (cons 2 (k (void))))
                          '()))
                 '(1 2)))

(assert! (equal? (reset (begin
                          (shift k (cons 1 (k (void))))
                          (list 2)))
                 '(1 2)))

(define (yield x)
  (shift k (cons x (k (void)))))

(assert! (equal? (reset (begin
                          (yield 1)
                          (yield 2)
                          (yield 3)
                          '())) ;; (list 1 2 3)))
                 '(1 2 3)))
