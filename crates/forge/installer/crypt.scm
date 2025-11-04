(require-builtin steel/hashes)

(provide hash-file
         hash-directory
         directories-equal?)

;;; -----------------------------------------------------------------
;;; Merge two lists of numbers which are already in increasing order

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
    (if (null? l) '() (if (null? (cdr l)) '() (cons (car (cdr l)) (even-numbers (cdr (cdr l))))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in odd positions

(define odd-numbers
  (lambda (l)
    (if (null? l)
        '()
        (if (null? (cdr l)) (list (car l)) (cons (car l) (odd-numbers (cdr (cdr l))))))))

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

(define (walk-files-sorted path func #:ignore [ignore-dirs #f])
  (cond
    [(is-file? path) (func path)]
    [(is-dir? path)
     (unless (and (set? ignore-dirs) (hashset-contains? ignore-dirs path))
       (for-each (lambda (x) (walk-files-sorted x func #:ignore ignore-dirs))
                 (merge-sort (read-dir path) #:comparator string<?)))]
    [else void]))

(define (hash-file-impl path #:hasher [hasher (md5-hasher)] #:buffer [buffer (bytevector)])
  (define input-file (open-input-file path))
  (define (loop)
    ;; Need to just read into a buffer instead?
    (define line (read-bytes 8192 input-file))
    (cond
      [(eof-object? line) hasher]
      [else
       (md5-hasher-update! hasher line)
       (loop)]))

  (loop)
  (close-input-port input-file))

(define (hash-file path)
  (~> path hash-file-impl md5-hasher->bytes))

(define (hash-directory path #:ignore [ignore-dirs #f])
  (define hasher (md5-hasher))
  (walk-files-sorted path (lambda (p) (hash-file-impl p #:hasher hasher)) #:ignore ignore-dirs)
  (md5-hasher->bytes hasher))

(define (directories-equal? a b #:ignore-a [ignore-dirs-a #f] #:ignore-b [ignore-dirs-b #f])
  (equal? (hash-directory a #:ignore ignore-dirs-a) (hash-directory b #:ignore ignore-dirs-b)))
