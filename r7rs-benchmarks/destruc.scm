(require "common.scm")
(require "mutable-lists.scm")

;;; DESTRUC -- Destructive operation benchmark.

(define (append-to-tail! x y)
  (if (null? x)
      y
      (let loop ((a x) (b (cdr x)))
        (if (null? b)
            (begin
              (set-cdr! a y)
              x)
            (loop b (cdr b))))))

(define (destructive n m)
  (let ((l (do ((i 10 (- i 1)) (a '() (cons '() a)))
               ((= i 0) a))))
    (do ((i n (- i 1)))
        ((= i 0) l)
      (cond ((null? (car l))
             (do ((l l (cdr l)))
                 ((null? l))
               (when (null? (car l))
                 (set-car! l (cons '() '())))
               (append-to-tail! (car l)
                                (do ((j m (- j 1)) (a '() (cons '() a)))
                                    ((= j 0) a)))))
            (else
             (do ((l1 l (cdr l1)) (l2 (cdr l) (cdr l2)))
                 ((null? l2))
               (set-cdr! (do ((j (quotient (length (car l2)) 2) (- j 1))
                              (a (car l2) (cdr a)))
                             ((zero? j) a)
                           (set-car! a i))
                         (let ((n (quotient (length (car l1)) 2)))
                           (cond ((= n 0)
                                  (set-car! l1 '())
                                  (car l1))
                                 (else
                                  (do ((j n (- j 1)) (a (car l1) (cdr a)))
                                      ((= j 1)
                                       (let ((x (cdr a)))
                                         (set-cdr! a '())
                                         x))
                                    (set-car! a i))))))))))))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "destruc"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3)
     count
     (lambda ()
       (destructive (hide count input1) (hide count input2)))
     (lambda (result) (equal? result output)))))

(with-input-from-file (bench-input "destruc") run-benchmark)
