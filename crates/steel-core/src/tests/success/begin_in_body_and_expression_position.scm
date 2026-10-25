;; A begin in body position shares its macros with the rest of the body. One
;; in expression position keeps them to itself.

(define (body-position)
  (begin
    (define-syntax one (syntax-rules () [(_) 1])))
  (one))
(assert! (= (body-position) 1))

(define-syntax which (syntax-rules () [(_) 'outer]))
(define (expression-position)
  (define inside
    (when #t
      (begin
        (define-syntax which (syntax-rules () [(_) 'inner]))
        (which))))
  (list inside (which)))
(assert! (equal? (expression-position) '(inner outer)))

(define (argument-position)
  (list (begin
          (define-syntax which (syntax-rules () [(_) 'arg]))
          (which))
        (which)))
(assert! (equal? (argument-position) '(arg outer)))
