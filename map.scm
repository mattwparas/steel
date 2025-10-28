;; The empty list? How to make it so it doesn't get passed in?
(define (map proc lst . lsts)
  (define (map-inner func accum lst)
    (if (empty? lst)
        (reverse accum)
        (map-inner func (cons (func (car lst)) accum) (cdr lst))))

  (define (map-multiple-inner func accum lsts)
    ;; Call `cdr` on each element, and then apply
    ;; the function with those args
    (error "todo"))

  (if (empty? lsts)
      (map-inner proc '() lst)

      (map-multiple-inner proc '() cons lst lsts)))
