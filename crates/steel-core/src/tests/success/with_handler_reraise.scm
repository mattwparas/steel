;; An error raised again from an inner handler reaches the enclosing handler
(define (inner thunk)
  (with-handler (lambda (err) (raise-error err)) (thunk)))

(define (outer thunk)
  (with-handler (lambda (err) (error-object-message err)) (inner thunk)))

(define (message-of thunk)
  (with-handler (lambda (err) (error-object-message err)) (thunk)))

(define scheme-error (lambda () (error "inner")))
(define native-error (lambda () (car '())))

(assert! (equal? (outer scheme-error) (message-of scheme-error)))
(assert! (equal? (outer native-error) (message-of native-error)))

;; The multi-expression form behaves the same
(define (outer-multi thunk)
  (with-handler (lambda (err) (error-object-message err))
                (with-handler (lambda (err) (raise-error err))
                              1
                              (thunk))))

(assert! (equal? (outer-multi scheme-error) (message-of scheme-error)))

;; Execution continues normally after the outer handler returns
(assert! (equal? (list (outer scheme-error) 'after) (list (message-of scheme-error) 'after)))
