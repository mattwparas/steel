(require (only-in "steel/result" map-ok Ok Err))

(define my-result (Ok 10))

(assert! (equal? (map-ok my-result (lambda (x) (+ x 10))) (Ok 20)))
(assert! (equal? (map-ok (Err 10) (lambda (x) (+ x 10))) (Err 10)))

(assert! (equal? (unwrap-ok my-result) 10))
