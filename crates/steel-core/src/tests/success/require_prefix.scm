(require (prefix-in result/ "steel/result"))

(define my-result (Ok 10))

(assert! (equal? (result/map-ok my-result (lambda (x) (+ x 10))) (result/Ok 20)))
(assert! (equal? (result/map-ok (result/Err 10) (lambda (x) (+ x 10))) (result/Err 10)))
