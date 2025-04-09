(provide walk-files
         file->string)

(define merge-lists
  (lambda (l1 l2 comparator)
    (if (null? l1)
        l2
        (if (null? l2)
            l1
            (if (comparator (car l1) (car l2))
                (cons (car l1) (merge-lists (cdr l1) l2 comparator))
                (cons (car l2) (merge-lists (cdr l2) l1 comparator)))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in even positions

(define even-numbers
  (lambda (l)
    (if (null? l)
        '()
        (if (null? (cdr l))
            '()
            (cons (car (cdr l)) (even-numbers (cdr (cdr l))))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in odd positions

(define odd-numbers
  (lambda (l)
    (if (null? l)
        '()
        (if (null? (cdr l))
            (list (car l))
            (cons (car l) (odd-numbers (cdr (cdr l))))))))

;;; ---------------------------------------------------------------------
;;; Use the procedures above to create a simple and efficient merge-sort

(define (merge-sort l #:comparator [comparator <])
  (if (null? l)
      l
      (if (null? (cdr l))
          l
          (merge-lists (merge-sort (odd-numbers l) #:comparator comparator)
                       (merge-sort (even-numbers l) #:comparator comparator)
                       comparator))))

(define (for-each func lst)
  (if (null? lst)
      void
      (begin
        (func (car lst))
        (when (null? lst)
          (return! void))
        (for-each func (cdr lst)))))

(define (file->string path)
  (let ([file (open-input-file path)]) (read-port-to-string file)))

;; Walk the file system, applying a function to each file found
(define/contract (walk-files path func)
  (->/c string? (->/c string? any/c) any/c)
  (cond
    [(is-file? path) (func path)]
    [(is-dir? path)
     (for-each (lambda (x) (walk-files x func)) (merge-sort (read-dir path) #:comparator string<?))]
    [else void]))
