(define (map function list1 . more-lists)
  (define (some? function list)
    (and (pair? list) (or (function (car list)) (some? function (cdr list)))))

  (define (map1 func accum lst)
    (if (null? lst) (reverse accum) (map1 func (cons (func (car lst)) accum) (cdr lst))))

  (define (map-many func accum lsts)
    (if (some? null? lsts)
        (reverse accum)
        (map-many func (cons (apply function (map1 car '() lsts)) accum) (map1 cdr '() lsts))))

  (if (null? more-lists)
      (map1 function '() list1)
      (let ([lists (cons list1 more-lists)])
        (if (some? null? lists) '() (map-many function '() lists)))))

(define (filter function lst)
  (define (filter-inner function accum lst)
    (if (null? lst)
        (reverse accum)
        (let ([next (car lst)])
          (if (function next)
              (filter-inner function (cons accum next) (cdr lst))
              (filter-inner function accum (cdr lst))))))

  (filter-inner function '() lst))
