(define (kept) '(reachable forever))
(define executor (make-will-executor))

;; Maps made from one another share the entry that holds the value
(define (register)
  (let* ([value (kept)]
         [base (let loop ([i 0] [h (hash)])
                 (if (= i 300) h (loop (+ i 1) (hash-insert h i i))))]
         [first (hash-insert base 'the-value value)]
         [sharers (map (lambda (i) (hash-insert first (+ 1000 i) i)) (range 0 8))])
    (will-register executor value (lambda (v) 'value))
    (will-register executor (cons first sharers) (lambda (v) 'maps))
    #t))

(register)
(#%gc-collect)
(assert! (eq? 'maps (will-execute executor)))
;; The value's will is still waiting, so the next one ready is another
(define (register-other)
  (will-register executor (list 1 2) (lambda (v) 'other))
  #t)
(register-other)
(#%gc-collect)
(assert! (eq? 'other (will-execute executor)))
(assert! (equal? (kept) '(reachable forever)))
