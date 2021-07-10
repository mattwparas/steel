(provide merge-sort)

(define (empty) '())

;;; Define a merge sort for sake of simplicity
(define (merge-lists l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [(< (car l1) (car l2))
         (cons (car l1) (merge-lists (cdr l1) l2))]
        [else
         (cons (car l2) (merge-lists (cdr l2) l1))]))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in even positions

(define (even-numbers l)
  (cond [(null? l) (empty)]
        [(null? (cdr l)) (empty)]
        [else
         (cons (car (cdr l)) (even-numbers (cdr (cdr l))))]))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in odd positions

(define (odd-numbers l)
  (cond [(null? l) (empty)]
        [(null? (cdr l)) (cons (car l) (empty))]
        [else
         (cons (car l) (odd-numbers (cdr (cdr l))))]))

;;; ---------------------------------------------------------------------
;;; Use the procedures above to create a simple and efficient merge-sort

(define (merge-sort l)
  (cond [(null? l) l]
        [(null? (cdr l)) l]
        [else
         (merge-lists
          (merge-sort (odd-numbers l))
          (merge-sort (even-numbers l)))]))