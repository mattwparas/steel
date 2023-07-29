(assert! (not (try-list-ref (list 10 20 30 40) 100)))

(assert! (equal? (take (range 0 10) 3) (list 0 1 2)))

(assert! (equal? '() (append)))

(assert! (empty? '()))
(assert! (not (empty? (range 0 10))))

;; Type handling
(with-handler
 (lambda (err) void)
 (append (vector 10) (vector 20))
 (assert!
  #t)) ;; TODO have to use non const values here, or don't evaluate constant folding inside of a with-handler?
(with-handler (lambda (err) void)
              (first (hash)) ;; TODO have to use non const values here
              (assert! #t))
(with-handler (lambda (err) void) (reverse (hash)) (assert! #t))
(with-handler (lambda (err) void) (length (vector)) (assert! #t))
(with-handler (lambda (err) void) (empty? 100) (assert! #t))
