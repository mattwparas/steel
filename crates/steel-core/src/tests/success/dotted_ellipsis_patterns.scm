(define-syntax parts
  (syntax-rules ()
    [(_ (name args ... . rest)) (list 'name '(args ...) 'rest)]))

;; The rest of a proper list is empty, however few elements it has
(assert! (equal? (parts (f)) '(f () ())))
(assert! (equal? (parts (f a b)) '(f (a b) ())))
(assert! (equal? (parts (f a . r)) '(f (a) r)))
(assert! (equal? (parts (f . r)) '(f () r)))
(assert! (equal? (parts (f a b . r)) '(f (a b) r)))
