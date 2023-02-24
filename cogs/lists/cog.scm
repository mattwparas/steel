(define package-name 'steel/lists)
(define version "0.1.0")

;; Core library, requires no dependencies
(define dependencies '())

(define (split-last-loop accum lst)
    (if (empty? (cdr lst))
        (list (reverse accum) (car lst))
        (loop (cons (car lst) accum) (cdr lst))))

;; Given a list, splits off the last argument, returns as a pair
(define (split-last lst)
  (split-last-loop '() lst))