;;; Simple 'Tree' implementation in Steel
;;; Relies on the use of bindings to the file system through Rust

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
  (cond [(null? l) '()]
        [(null? (cdr l)) '()]
        [else
         (cons (car (cdr l)) (even-numbers (cdr (cdr l))))]))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in odd positions

(define (odd-numbers l)
  (cond [(null? l) '()]
        [(null? (cdr l)) (list (car l))]
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


(define (flatten x)
  (cond [(null? x) '()]
        [(not (list? x)) (list x)]
        [else (append (flatten (car x))
                      (flatten (cdr x)))]))


;; Simple tree implementation
;; Walks the file structure and prints without much fancy formatting
;; Returns a list of the visited files for convenience
(define (tree p)
  (define (tree-rec path padding)
    (define name (file-name path))
    (displayln (string-append padding name))
    (cond [(is-file? path) name]
          [(is-dir? path)
           (map (fn (x)
                  (tree-rec x (string-append padding "    ")))
                (merge-sort (read-dir path)))]
          [else void]))
  (flatten (tree-rec p "")))


(define p "/Users/mwparas/Documents/MatSci201")

(tree p)
