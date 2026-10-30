;; Phase 1 code that isn't a transformer requires match for syntax, as in Racket
(require (for-syntax "#%private/steel/match"))
(begin-for-syntax
  (define (classify x)
    (match x
      [1 'one]
      [(list a b ...) (list a b)]
      [_ 'other])))
(define-syntax (classify-all stx)
  (datum->syntax stx (list 'quote (list (classify 1) (classify '(1 2 3)) (classify 2)))))
(assert! (equal? (classify-all) '(one (1 (2 3)) other)))
