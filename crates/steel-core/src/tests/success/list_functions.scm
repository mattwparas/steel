(assert! (not (try-list-ref (list 10 20 30 40) 100)))

(assert! (equal? (take (range 0 10) 3) (list 0 1 2)))

(assert! (equal? '() (append)))

(assert! (empty? '()))
(assert! (not (empty? (range 0 10))))

;; Type handling
(with-handler (lambda (err) void) (append 10 20) (assert! #t))
(with-handler (lambda (err) void) (first 10) (assert! #t))
(with-handler (lambda (err) void) (reverse 100) (assert! #t))
(with-handler (lambda (err) void) (length 10) (assert! #t))
(with-handler (lambda (err) void) (empty? 100) (assert! #t))
