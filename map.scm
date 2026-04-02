(provide map2)

(define (map2 function list1 . more-lists)
  (define (some? function list)
    (and (pair? list) (or (function (car list)) (some? function (cdr list)))))

  (define (map1 func accum lst)
    (if (null? lst)
        (reverse accum)
        (map1 func (cons (func (car lst)) accum) (cdr lst))))

  (define (map-many func accum lsts)
    (if (some? null? lsts)
        (reverse accum)
        (map-many func (cons (apply func (map1 car '() lsts)) accum) (map1 cdr '() lsts))))

  (if (null? more-lists)
      (map1 function '() list1)
      (let ([lists (cons list1 more-lists)])
        (if (some? null? lists)
            '()
            (map-many function '() lists)))))

; ; (define (map1 func accum lst)
; ;   (if (null? lst) (reverse accum) (map1 func (cons (func (car lst)) accum) (cdr lst))))

; ; ; (provide map-new)
; ; (define (map-new func lst)
; ;   (map1 func '() lst))

(define r (range 0 100000))

(define results (mutable-vector))

(define (loop x)
  (if (= x 100)
      void
      (begin
        (vector-push! results (map (lambda (x) (+ x 1)) r))
        (loop (+ x 1)))))

(provide run)
(define (run)
  (loop 0))

(provide map1)
;; Couple things:
;; - Move the list library in to this library
;; - Encode the null check into the branching. We know
;;   that on the
(define (map1 func accum lst)
  ;; A couple neat optimizations we might be able to make:
  ;; If the value takes the register by mut, we can actually
  ;; just leave it in place. Return a new register. In the event
  ;; we actually need this, we'll reify it at the event.
  ;;
  ;; This can actually be the case for multiple branches.
  (if (null? lst)
      (reverse accum)

      (map1 func (cons (func (car lst)) accum) (cdr lst))))
