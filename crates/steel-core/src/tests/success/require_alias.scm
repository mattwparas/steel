(require (only-in "steel/result" map-ok (Ok Foo) (Err Bar)))

(define my-result (Foo 10))

(assert! (equal? (map-ok my-result (lambda (x) (+ x 10))) (Foo 20)))
(assert! (equal? (map-ok (Bar 10) (lambda (x) (+ x 10))) (Bar 10)))
