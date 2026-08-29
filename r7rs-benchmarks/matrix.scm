(require "common.scm")

;;; MATRIX -- Obtained from Andrew Wright.

;;; We need R6RS div and mod for this benchmark.

(define (div x y)
  (cond ((and (exact-integer? x)
              (exact-integer? y)
              (>= x 0))
         (quotient x y))
        ((< y 0)
         ;; x < 0, y < 0
         (let* ((q (quotient x y))
                (r (- x (* q y))))
           (if (= r 0)
               q
               (+ q 1))))
        (else
         ;; x < 0, y > 0
         (let* ((q (quotient x y))
                (r (- x (* q y))))
           (if (= r 0)
               q
               (- q 1))))))

(define (mod x y)
  (cond ((and (exact-integer? x)
              (exact-integer? y)
              (>= x 0))
         (remainder x y))
        ((< y 0)
         ;; x < 0, y < 0
         (let* ((q (quotient x y))
                (r (- x (* q y))))
           (if (= r 0)
               0
               (- r y))))
        (else
         ;; x < 0, y > 0
         (let* ((q (quotient x y))
                (r (- x (* q y))))
           (if (= r 0)
               0
               (+ r y))))))

;; Chez-Scheme compatibility stuff:


;; Test that a matrix with entries in {+1, -1} is maximal among the matricies
;; obtainable by
;;       re-ordering the rows
;;       re-ordering the columns
;;       negating any subset of the columns
;;       negating any subset of the rows
;; Where we compare two matricies by lexicographically comparing the first row,
;; then the next to last, etc., and we compare a row by lexicographically
;; comparing the first entry, the second entry, etc., and we compare two
;; entries by +1 > -1.
;; Note, this scheme obeys the useful fact that if (append mat1 mat2) is
;; maximal, then so is mat1.  Thus, we can build up maximal matricies
;; row by row.
;;
;; Once you have chosen the row re-ordering so that you know which row goes
;; last, the set of columns to negate is fixed (since the last row must be
;; all +1's).
;;
;; Note, the column ordering is really totally determined as follows:
;;       all columns for which the second row is +1 must come before all
;;               columns for which the second row is -1.
;;       among columns for which the second row is +1, all columns for which
;;               the third row is +1 come before those for which the third is
;;               -1, and similarly for columns in which the second row is -1.
;;       etc
;; Thus, each succeeding row sorts columns withing refinings equivalence
;; classes.
;;
;; Maximal? assumes that mat has atleast one row, and that the first row
;; is all +1's.
(define maximal?
  (lambda (mat)
    (let pick-first-row
        ((first-row-perm
          (gen-perms mat)))
      (if first-row-perm
          (and (zunda first-row-perm mat)
               (pick-first-row (first-row-perm 'brother)))
          #t))))

(define zunda
  (lambda (first-row-perm mat)
    (let* ((first-row
            (first-row-perm 'now))
           (number-of-cols
            (length first-row))
           (make-row->func
            (lambda (if-equal if-different)
              (lambda (row)
                (let ((vec
                       (make-vector number-of-cols)))
                  (do ((i 0 (+ i 1))
                       (first first-row
                              (cdr first))
                       (row row
                            (cdr row)))
                      ((= i number-of-cols))
                    (vector-set! vec
                                 i
                                 (if (= (car first) (car row))
                                     if-equal
                                     if-different)))
                  (lambda (i)
                    (vector-ref vec i))))))
           (mat
            (cdr mat)))
      (zebra (first-row-perm 'child)
             (make-row->func 1 -1)
             (make-row->func -1 1)
             mat
             number-of-cols))))


(define zebra
  (lambda (row-perm row->func+ row->func- mat number-of-cols)
    (let _-*-
        ((row-perm
          row-perm)
         (mat
          mat)
         (partitions
          (list (miota number-of-cols))))
      (or (not row-perm)
          (and
           (zulu (car mat)
                 (row->func+ (row-perm 'now))
                 partitions
                 (lambda (new-partitions)
                   (_-*- (row-perm 'child)
                         (cdr mat)
                         new-partitions)))
           (zulu (car mat)
                 (row->func- (row-perm 'now))
                 partitions
                 (lambda (new-partitions)
                   (_-*- (row-perm 'child)
                         (cdr mat)
                         new-partitions)))
           (let ((new-row-perm
                  (row-perm 'brother)))
             (or (not new-row-perm)
                 (_-*- new-row-perm
                       mat
                       partitions))))))))


(define zulu
  (let ((cons-if-not-null
         (lambda (lhs rhs)
           (if (null? lhs)
               rhs
               (cons lhs rhs)))))
    (lambda (old-row new-row-func partitions equal-cont)
      (let _-*-
          ((p-in
            partitions)
           (old-row
            old-row)
           (rev-p-out
            '()))
        (let _-split-
            ((partition
              (car p-in))
             (old-row
              old-row)
             (plus
              '())
             (minus
              '()))
          (if (null? partition)
              (let _-minus-
                  ((old-row
                    old-row)
                   (m
                    minus))
                (if (null? m)
                    (let ((rev-p-out
                           (cons-if-not-null
                            minus
                            (cons-if-not-null
                             plus
                             rev-p-out)))
                          (p-in
                           (cdr p-in)))
                      (if (null? p-in)
                          (equal-cont (reverse rev-p-out))
                          (_-*- p-in old-row rev-p-out)))
                    (or (= 1 (car old-row))
                        (_-minus- (cdr old-row)
                                  (cdr m)))))
              (let ((next
                     (car partition)))
                (case (new-row-func next)
                  ((1)
                   (and (= 1 (car old-row))
                        (_-split- (cdr partition)
                                  (cdr old-row)
                                  (cons next plus)
                                  minus)))
                  ((-1)
                   (_-split- (cdr partition)
                             old-row
                             plus
                             (cons next minus)))))))))))

(define all?
  (lambda (ok? lst)
    (let _-*-
        ((lst
          lst))
      (or (null? lst)
          (and (ok? (car lst))
               (_-*- (cdr lst)))))))

(define gen-perms
  (lambda (objects)
    (let _-*-
        ((zulu-future
          objects)
         (past
          '()))
      (if (null? zulu-future)
          #f
          (lambda (msg)
            (case msg
              ((now)
               (car zulu-future))
              ((brother)
               (_-*- (cdr zulu-future)
                     (cons (car zulu-future)
                           past)))
              ((child)
               (gen-perms
                (fold past cons (cdr zulu-future))))
              ((puke)
               (cons (car zulu-future)
                     (fold past cons (cdr zulu-future))))
              (else
               (error 'gen-perms "Bad msg: ~a" msg))))))))

(define fold
  (lambda (lst folder state)
    (let _-*-
        ((lst
          lst)
         (state
          state))
      (if (null? lst)
          state
          (_-*- (cdr lst)
                (folder (car lst)
                        state))))))

(define miota
  (lambda (len)
    (let _-*-
        ((i 0))
      (if (= i len)
          '()
          (cons i
                (_-*- (+ i 1)))))))

(define proc->vector
  (lambda (size proc)
    (let ((res
           (make-vector size)))
      (do ((i 0
              (+ i 1)))
          ((= i size))
        (vector-set! res
                     i
                     (proc i)))
      res)))

;; Given a prime number P, return a procedure which, given a `maker' procedure,
;; calls it on the operations for the field Z/PZ.
(define make-modular
  (lambda (modulus)
    (let* ((reduce
            (lambda (x)
              (mod x modulus)))
           (coef-zero?
            (lambda (x)
              (zero? (reduce x))))
           (coef-+
            (lambda (x y)
              (reduce (+ x y))))
           (coef-negate
            (lambda (x)
              (reduce (- x))))
           (coef-*
            (lambda (x y)
              (reduce (* x y))))
           (coef-recip
            (let ((inverses
                   (proc->vector (- modulus 1)
                                 (lambda (i)
                                   (extended-gcd (+ i 1)
                                                 modulus
                                                 (lambda (gcd inverse ignore)
                                                   inverse))))))
              ;; Coef-recip.
              (lambda (x)
                (let ((x
                       (reduce x)))
                  (vector-ref inverses (- x 1)))))))
      (lambda (maker)
        (maker 0 ;; coef-zero
               1 ;; coef-one
               coef-zero?
               coef-+
               coef-negate
               coef-*
               coef-recip)))))

;; Extended Euclidean algorithm.
;; (extended-gcd a b cont) computes the gcd of a and b, and expresses it
;; as a linear combination of a and b.  It returns calling cont via
;;       (cont gcd a-coef b-coef)
;; where gcd is the GCD and is equal to a-coef * a + b-coef * b.
(define extended-gcd
  (let ((n->sgn/abs
         (lambda (x cont)
           (if (>= x 0)
               (cont 1 x)
               (cons -1 (- x))))))
    (lambda (a b cont)
      (n->sgn/abs a
                  (lambda (p-a p)
                    (n->sgn/abs b
                                (lambda (q-b q)
                                  (let _-*-
                                      ((p
                                        p)
                                       (p-a
                                        p-a)
                                       (p-b
                                        0)
                                       (q
                                        q)
                                       (q-a
                                        0)
                                       (q-b
                                        q-b))
                                    (if (zero? q)
                                        (cont p p-a p-b)
                                        (let ((mult
                                               (div p q)))
                                          (_-*- q
                                                q-a
                                                q-b
                                                (- p (* mult q))
                                                (- p-a (* mult q-a))
                                                (- p-b (* mult q-b)))))))))))))

;; Given elements and operations on the base field, return a procedure which
;; computes the row-reduced version of a matrix over that field.  The result
;; is a list of rows where the first non-zero entry in each row is a 1 (in
;; the coefficient field) and occurs to the right of all the leading non-zero
;; entries of previous rows.  In particular, the number of rows is the rank
;; of the original matrix, and they have the same row-space.
;; The items related to the base field which are needed are:
;;       coef-zero       additive identity
;;       coef-one        multiplicative identity
;;       coef-zero?      test for additive identity
;;       coef-+          addition (two args)
;;       coef-negate     additive inverse
;;       coef-*          multiplication (two args)
;;       coef-recip      multiplicative inverse
;; Note, matricies are stored as lists of rows (i.e., lists of lists).
(define make-row-reduce
  (lambda (coef-zero coef-one coef-zero? coef-+ coef-negate coef-* coef-recip)
    (lambda (mat)
      (let _-*-
          ((mat
            mat))
        (if (or (null? mat)
                (null? (car mat)))
            '()
            (let _-**-
                ((in
                  mat)
                 (out
                  '()))
              (if (null? in)
                  (map
                   (lambda (x)
                     (cons coef-zero x))
                   (_-*- out))
                  (let* ((prow
                          (car in))
                         (pivot
                          (car prow))
                         (prest
                          (cdr prow))
                         (in
                          (cdr in)))
                    (if (coef-zero? pivot)
                        (_-**- in
                               (cons prest out))
                        (let ((zap-row
                               (map
                                (let ((mult
                                       (coef-recip pivot)))
                                  (lambda (x)
                                    (coef-* mult x)))
                                prest)))
                          (cons (cons coef-one zap-row)
                                (map
                                 (lambda (x)
                                   (cons coef-zero x))
                                 (_-*-
                                  (fold in
                                        (lambda (row mat)
                                          (cons
                                           (let ((first-col
                                                  (car row))
                                                 (rest-row
                                                  (cdr row)))
                                             (if (coef-zero? first-col)
                                                 rest-row
                                                 (map
                                                  (let ((mult
                                                         (coef-negate first-col)))
                                                    (lambda (f z)
                                                      (coef-+ f
                                                              (coef-* mult z))))
                                                  rest-row
                                                  zap-row)))
                                           mat))
                                        out))))))))))))))


;; Given elements and operations on the base field, return a procedure which
;; when given a matrix and a vector tests to see if the vector is in the
;; row-space of the matrix.  This returned function is curried.
;; The items related to the base field which are needed are:
;;       coef-zero       additive identity
;;       coef-one        multiplicative identity
;;       coef-zero?      test for additive identity
;;       coef-+          addition (two args)
;;       coef-negate     additive inverse
;;       coef-*          multiplication (two args)
;;       coef-recip      multiplicative inverse
;; Note, matricies are stored as lists of rows (i.e., lists of lists).
(define make-in-row-space?
  (lambda (coef-zero coef-one coef-zero? coef-+ coef-negate coef-* coef-recip)
    (let ((row-reduce
           (make-row-reduce coef-zero
                            coef-one
                            coef-zero?
                            coef-+
                            coef-negate
                            coef-*
                            coef-recip)))
      (lambda (mat)
        (let ((mat
               (row-reduce mat)))
          (lambda (row)
            (let _-*-
                ((row
                  row)
                 (mat
                  mat))
              (if (null? row)
                  #t
                  (let ((r-first
                         (car row))
                        (r-rest
                         (cdr row)))
                    (cond ((coef-zero? r-first)
                           (_-*- r-rest
                                 (map cdr
                                      (if (or (null? mat)
                                              (coef-zero? (caar mat)))
                                          mat
                                          (cdr mat)))))
                          ((null? mat)
                           #f)
                          (else
                           (let* ((zap-row
                                   (car mat))
                                  (z-first
                                   (car zap-row))
                                  (z-rest
                                   (cdr zap-row))
                                  (mat
                                   (cdr mat)))
                             (if (coef-zero? z-first)
                                 #f
                                 (_-*-
                                  (map
                                   (let ((mult
                                          (coef-negate r-first)))
                                     (lambda (r z)
                                       (coef-+ r
                                               (coef-* mult z))))
                                   r-rest
                                   z-rest)
                                  (map cdr mat)))))))))))))))


;; Given a prime number, return a procedure which takes integer matricies
;; and returns their row-reduced form, modulo the prime.
(define make-modular-row-reduce
  (lambda (modulus)
    ((make-modular modulus)
     make-row-reduce)))


(define make-modular-in-row-space?
  (lambda (modulus)
    ((make-modular modulus)
     make-in-row-space?)))



;; Usual utilities.



;; Given a bound, find a prime greater than the bound.
(define find-prime
  (lambda (bound)
    (if (> 2 bound)
        2
        (let ((primes (make-vector 64 0)) (count 1))
          (vector-set! primes 0 2)
          (letrec ((is-next-prime?
                    (lambda (trial)
                      (let _-*- ((i 0))
                        (or (>= i count)
                            (let ((p (vector-ref primes i)))
                              (or (< trial (* p p))
                                  (and (not (zero? (mod trial p)))
                                       (_-*- (+ i 1)))))))))
                   (add-prime!
                    (lambda (p)
                      (if (= count (vector-length primes))
                          (let ((bigger (make-vector (* 2 (vector-length primes)) 0)))
                            (let _-*- ((i 0))
                              (if (< i count)
                                  (begin
                                    (vector-set! bigger i (vector-ref primes i))
                                    (_-*- (+ i 1)))))
                            (set! primes bigger)))
                      (vector-set! primes count p)
                      (set! count (+ count 1)))))
            (let _-*- ((trial 3))
              (if (is-next-prime? trial)
                  (begin
                    (add-prime! trial)
                    (if (> trial bound) trial (_-*- (+ trial 2))))
                  (_-*- (+ trial 2)))))))))

;; Given the size of a square matrix consisting only of +1's and -1's,
;; return an upper bound on the determinant.
(define det-upper-bound
  (lambda (size)
    (let ((main-part
           (expt size
                 (div size 2))))
      (if (even? size)
          main-part
          (* main-part
             (do ((i 0 (+ i 1)))
                 ((>= (* i i) size)
                  i)))))))

;; Fold over all maximal matrices.
(define go
  (lambda (number-of-cols inv-size folder state)
    (let* ((in-row-space?
            (make-modular-in-row-space?
             (find-prime
              (det-upper-bound inv-size))))
           (make-tester
            (lambda (mat)
              (let ((tests
                     (let ((old-mat
                            (cdr mat))
                           (new-row
                            (car mat)))
                       (fold-over-subs-of-size old-mat
                                               (- inv-size 2)
                                               (lambda (sub tests)
                                                 (cons
                                                  (in-row-space?
                                                   (cons new-row sub))
                                                  tests))
                                               '()))))
                (lambda (row)
                  (let _-*-
                      ((tests
                        tests))
                    (and (not (null? tests))
                         (or ((car tests) row)
                             (_-*- (cdr tests)))))))))
           (all-rows ;; all rows starting with +1 in decreasing order
            (fold
             (fold-over-rows (- number-of-cols 1)
                             cons
                             '())
             (lambda (row rows)
               (cons (cons 1 row)
                     rows))
             '())))
      (let _-*-
          ((number-of-rows
            1)
           (rev-mat
            (list
             (car all-rows)))
           (possible-future
            (cdr all-rows))
           (state
            state))
        (let ((zulu-future
               (remove-in-order
                (if (< number-of-rows inv-size)
                    (in-row-space? rev-mat)
                    (make-tester rev-mat))
                possible-future)))
          (if (null? zulu-future)
              (folder (reverse rev-mat)
                      state)
              (let _-**-
                  ((zulu-future
                    zulu-future)
                   (state
                    state))
                (if (null? zulu-future)
                    state
                    (let ((rest-of-future
                           (cdr zulu-future)))
                      (_-**- rest-of-future
                             (let* ((first
                                     (car zulu-future))
                                    (new-rev-mat
                                     (cons first rev-mat)))
                               (if (maximal? (reverse new-rev-mat))
                                   (_-*- (+ number-of-rows 1)
                                         new-rev-mat
                                         rest-of-future
                                         state)
                                   state))))))))))))

(define go-folder
  (lambda (mat bsize.blen.blist)
    (let ((bsize
           (car bsize.blen.blist))
          (size
           (length mat)))
      (if (< size bsize)
          bsize.blen.blist
          (let ((blen
                 (cadr bsize.blen.blist))
                (blist
                 (cddr bsize.blen.blist)))
            (if (= size bsize)
                (let ((blen
                       (+ blen 1)))
                  ;;                       (if
                  ;;                          (let _-*-
                  ;;                               ((blen
                  ;;                                   blen))
                  ;;                             (or (< blen 10)
                  ;;                                 (and (zero? (mod blen 10))
                  ;;                                      (_-*- (div blen 10)))))
                  ;;
                  ;;                          (begin
                  ;;                             (display blen)
                  ;;                             (display " of size ")
                  ;;                             (display bsize)
                  ;;                             (newline)))

                  (cons bsize
                        (cons blen
                              (cond ((< blen 3000)
                                     (cons mat blist))
                                    ((= blen 3000)
                                     (cons "..." blist))
                                    (else
                                     blist)))))
                ;;                    (begin
                ;;                       (newline)
                ;;                       (display "First of size ")
                ;;                       (display size)
                ;;                       (display ":")
                ;;                       (newline)
                ;;                       (for-each
                ;;                          (lambda (row)
                ;;                             (display " ")
                ;;                             (for-each
                ;;                                (lambda (e)
                ;;                                   (case e
                ;;                                      ((1)
                ;;                                         (display "  1"))
                ;;                                      ((-1)
                ;;                                         (display " -1"))))
                ;;                                row)
                ;;                             (newline))
                ;;                          mat)

                (list size 1 mat)))))))

(define really-go
  (lambda (number-of-cols inv-size)
    (cddr
     (go number-of-cols
         inv-size
         go-folder
         (list -1 -1)))))

(define remove-in-order
  (lambda (remove? lst)
    (reverse
     (fold lst
           (lambda (e lst)
             (if (remove? e)
                 lst
                 (cons e lst)))
           '()))))

;; The first fold-over-rows is slower than the second one, but folds
;; over rows in lexical order (large to small).
(define fold-over-rows
  (lambda (number-of-cols folder state)
    (if (zero? number-of-cols)
        (folder '()
                state)
        (fold-over-rows (- number-of-cols 1)
                        (lambda (tail state)
                          (folder (cons -1 tail)
                                  state))
                        (fold-over-rows (- number-of-cols 1)
                                        (lambda (tail state)
                                          (folder (cons 1 tail)
                                                  state))
                                        state)))))

;; Fold over subsets of a given size.
(define fold-over-subs-of-size
  (lambda (universe size folder state)
    (let ((usize
           (length universe)))
      (if (< usize size)
          state
          (let _-*-
              ((size
                size)
               (universe
                universe)
               (folder
                folder)
               (csize
                (- usize size))
               (state
                state))
            (cond ((zero? csize)
                   (folder universe state))
                  ((zero? size)
                   (folder '() state))
                  (else
                   (let ((first-u
                          (car universe))
                         (rest-u
                          (cdr universe)))
                     (_-*- size
                           rest-u
                           folder
                           (- csize 1)
                           (_-*- (- size 1)
                                 rest-u
                                 (lambda (tail state)
                                   (folder (cons first-u tail)
                                           state))
                                 csize
                                 state))))))))))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "matrix"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3)
     count
     (lambda () (really-go (hide count input1) (hide count input2)))
     (lambda (result) (equal? result output)))))

(with-input-from-file (bench-input "matrix") run-benchmark)
