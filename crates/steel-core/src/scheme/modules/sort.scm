(provide sort)

(define (list->mutable-vector lst)
  (let ([v (mutable-vector)])
    (for-each (lambda (i) (vector-push! v i)) lst)
    v))

(define (sort l less?)
  (if (#%function-pointer? less?)
      (#%list-sort l less?)
      (begin
        (define v (list->mutable-vector l))
        (quick-1 v less?)
        (mutable-vector->list v))))

(define (quick-1 v less?)
  (define (helper left right)
    (if (< left right)
        (let ([median (partition v left right less?)])
          (if (< (- median left) (- right median))
              (begin
                (helper left (- median 1))
                (helper (+ median 1) right))
              (begin
                (helper (+ median 1) right)
                (helper left (- median 1)))))
        v))

  (define res (helper 0 (- (vector-length v) 1)))

  res)

(define (uploop v less? mid i right)
  (let ([i (+ i 1)])
    (if (and (< i right) (less? (vector-ref v i) mid))
        (uploop v less? mid i right)
        i)))

;; Same goes for down here
(define (downloop v less? mid j left)
  (let ([j (- j 1)])
    (if (and (> j left) (less? mid (vector-ref v j)))
        (downloop v less? mid j left)
        j)))

(define (ploop v less? left right mid i j)
  (let* ([i (uploop v less? mid i right)]
         [j (downloop v less? mid j left)])
    (let ([tmp (vector-ref v i)])
      ; (vector-set! v i (vector-ref v j))
      (vector-swap! v i j)
      (vector-set! v j tmp)
      (if (< i j)
          (ploop v less? left right mid i j)
          (begin
            (vector-swap! v j i)
            (vector-swap! v i right)
            (vector-set! v right tmp)
            i)))))

;; This is super duper slow?
(define (partition v left right less?)

  (let ([mid (vector-ref v right)])

    ;; Then, we should also implement a pass that does:
    ;; (vector-set! v j (vector-ref v i)) => (vector-swap v j i)

    (ploop v less? left right mid (- left 1) right)))

;;; Hansen's original code for this benchmark used Larceny's
;;; predefined random procedure.  When Marc Feeley modified
;;; Hansen's benchmark for the Gambit benchmark suite, however,
;;; he added a specific random number generator taken from an
;;; article in CACM.  Feeley's generator used bignums, and was
;;; extremely slow, causing the Gambit version of this benchmark
;;; to spend nearly all of its time generating the random numbers.
;;; For a benchmark called quicksort to become a bignum benchmark
;;; was very misleading, so Clinger left Feeley's version of this
;;; benchmark out of the Larceny benchmark suite.
;;;
;;; The following random number generator is much better and
;;; faster than the one used in the Gambit benchmark.  See
;;;
;;; http://srfi.schemers.org/srfi-27/mail-archive/msg00000.html
;;; http://www.math.purdue.edu/~lucier/random/random.scm

;;; A uniform [0,1] random number generator; is
;;; Pierre L'Ecuyer's generator from his paper
;;; "Good parameters and implementations for combined multiple
;;; recursive random number generators"
;;; available at his web site http://www.iro.umontreal.ca/~lecuyer

(define seed-set! #f)
(define seed-ref #f)
(define random-flonum #f)

(let ([norm 2.328306549295728e-10]
      [m1 4294967087.0]
      [m2 4294944443.0]
      [a12 1403580.0]
      [a13n 810728.0]
      [a21 527612.0]
      [a23n 1370589.0]
      [seed (vector 1.0 0.0 0.0 1.0 0.0 0.0)]) ;; will be mutated

  ;; uses no conversions between flonums and fixnums.

  (set! random-flonum
        (lambda ()
          (let ([seed seed]) ;; make it local
            (let ([p1 (- (* a12 (vector-ref seed 1)) (* a13n (vector-ref seed 0)))]
                  [p2 (- (* a21 (vector-ref seed 5)) (* a23n (vector-ref seed 3)))])
              (let ([k1 (truncate (/ p1 m1))]
                    [k2 (truncate (/ p2 m2))]
                    [ignore1 (vector-set! seed 0 (vector-ref seed 1))]
                    [ignore3 (vector-set! seed 3 (vector-ref seed 4))])
                (let ([p1 (- p1 (* k1 m1))]
                      [p2 (- p2 (* k2 m2))]
                      [ignore2 (vector-set! seed 1 (vector-ref seed 2))]
                      [ignore4 (vector-set! seed 4 (vector-ref seed 5))])
                  (let ([p1 (if (< p1 0.0)
                                (+ p1 m1)
                                p1)]
                        [p2 (if (< p2 0.0)
                                (+ p2 m2)
                                p2)])
                    (vector-set! seed 2 p1)
                    (vector-set! seed 5 p2)
                    (if (<= p1 p2)
                        (* norm (+ (- p1 p2) m1))
                        (* norm (- p1 p2))))))))))

  (set! seed-ref (lambda () (vector->list seed)))

  (set! seed-set! (lambda l (set! seed (list->vector l)))))

(define (random n)
  (exact (truncate (* (inexact n) (random-flonum)))))
