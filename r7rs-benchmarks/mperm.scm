;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File:         perm9.sch
;; Description:  memory system benchmark using Zaks's permutation generator
;; Author:       Lars Hansen, Will Clinger, and Gene Luks
;; Created:      18-Mar-94
;; Language:     Scheme
;; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 940720 / lth Added some more benchmarks for the thesis paper.
;; 970215 / wdc Increased problem size from 8 to 9; improved tenperm9-benchmark.
;; 970531 / wdc Cleaned up for public release.
;; 000820 / wdc Added the MpermNKL benchmark; revised for new run-benchmark.
;; 071127 / wdc Simplified and ported for R6RS.

(require "common.scm")

;; This benchmark is in three parts.  Each tests a different aspect of
;; the memory system.
;;
;;    storage allocation (while generating no garbage)
;;    storage allocation and garbage collection at equilibrium
;;    traversal of a large, linked, self-sharing structure
;;
;; The perm9 benchmark generates a list of all 362880 permutations of
;; the first 9 integers, allocating 1349288 pairs (typically 10,794,304
;; bytes), all of which goes into the generated list.  (That is, the
;; perm9 benchmark generates absolutely no garbage.)  This represents
;; a savings of about 63% over the storage that would be required by
;; an unshared list of permutations.  The generated permutations are
;; in order of a grey code that bears no obvious relationship to a
;; lexicographic order.
;;
;; The tenperm9 benchmark repeats the perm9 benchmark 10 times, so it
;; allocates and reclaims 13492880 pairs (typically 107,943,040 bytes).
;; The live storage peaks at twice the storage that is allocated by the
;; perm9 benchmark.  At the end of each iteration, the oldest half of
;; the live storage becomes garbage.  Object lifetimes are distributed
;; uniformly between 10.3 and 20.6 megabytes.
;;
;; The tenperm9 benchmark is the perm10:9:2:1 special case of the
;; MpermNKL benchmark, which allocates a queue of size K and then
;; performs M iterations of the following operation:  Fill the queue
;; with individually computed copies of all permutations of a list of
;; size N, and then remove the oldest L copies from the queue.  At the
;; end of each iteration, the oldest L/K of the live storage becomes
;; garbage, and object lifetimes are distributed uniformly between two
;; volumes that depend upon N, K, and L.
;;
;; As a check on the result, and to preclude overly sophisticated
;; compiler optimizations,  we compute the sum of the permuted
;; integers over all permutations.
;;
;; The benchmarks are run by calling the following procedure:
;;
;;    (MpermNKL-benchmark M N K L)
;;
;; This benchmark assumes that
;;
;;    (RUN-BENCHMARK <string> <count> <thunk> <predicate>)
;;
;; reports the time required to call <thunk> the number of times
;; specified by <count>, and uses <predicate> to test whether the
;; result returned by <thunk> is correct.

;; Date: Thu, 17 Mar 94 19:43:32 -0800
;; From: luks@sisters.cs.uoregon.edu
;; To: will
;; Subject: Pancake flips
;;
;; Procedure P_n generates a grey code of all perms of n elements
;; on top of stack ending with reversal of starting sequence
;;
;; F_n is flip of top n elements.
;;
;;
;; procedure P_n
;;
;;   if n>1 then
;;     begin
;;        repeat   P_{n-1},F_n   n-1 times;
;;        P_{n-1}
;;     end
;;

(define (permutations x)
  (let ([x x]
        [perms (list x)])
    (define (P n)
      (when (> n 1)
        (do ((j (- n 1) (- j 1))) ((zero? j) (P (- n 1))) (P (- n 1)) (F n))))
    (define (F n)
      (set! x (revloop x n (list-tail x n)))
      (set! perms (cons x perms)))
    (define (revloop x n y)
      (if (zero? n) y (revloop (cdr x) (- n 1) (cons (car x) y))))
    (define (list-tail x n)
      (if (zero? n) x (list-tail (cdr x) (- n 1))))
    (P (length x))
    perms))

;; Given a list of lists of numbers, returns the sum of the sums
;; of those lists.
;;
;; for (; x != NULL; x = x->rest)
;;     for (y = x->first; y != NULL; y = y->rest)
;;         sum = sum + y->first;

(define (sumlists x)
  (do ((x x (cdr x)) (sum 0 (do ((y (car x) (cdr y)) (sum sum (+ sum (car y)))) ((null? y) sum))))
      ((null? x) sum)))

(define (one..n n)
  (do ((n n (- n 1)) (p '() (cons n p))) ((zero? n) p)))

(define (tenperm . rest)
  (let ([n (if (null? rest) 9 (car rest))]) (MpermNKL-benchmark 10 n 2 1)))

(define (MpermNKL-benchmark m n k ell)
  (if (and (<= 0 m) (positive? n) (positive? k) (<= 0 ell k))
      (let ([id (string-append "mperm:"
                               (number->string m)
                               ":"
                               (number->string n)
                               ":"
                               (number->string k)
                               ":"
                               (number->string ell))]
            [queue (make-vector k '())])

        ;; Fills queue positions [i, j).

        (define (fill-queue i j)
          (when (< i j)
            (vector-set! queue i (permutations (one..n n)))
            (fill-queue (+ i 1) j)))

        ;; Removes ell elements from queue.

        (define (flush-queue)
          (let loop ([i 0])
            (when (< i k)
              (vector-set! queue i (let ([j (+ i ell)]) (if (< j k) (vector-ref queue j) '())))
              (loop (+ i 1)))))

        (fill-queue 0 (- k ell))
        (run-benchmark-x id
                         m
                         (lambda ()
                           (fill-queue (- k ell) k)
                           (flush-queue)
                           queue)
                         (lambda (q)
                           (define (factorial n)
                             (if (= n 0) 1 (* n (factorial (- n 1)))))
                           (let ([q0 (vector-ref q 0)]
                                 [qi (vector-ref q (max 0 (- k ell 1)))])
                             (and (or (and (null? q0) (null? qi))
                                      (and (pair? q0) (pair? qi) (equal? (car q0) (car qi))))
                                  (= (sumlists q0) (/ (* n (+ n 1) (factorial n)) 2)))))))
      (begin
        (display "Incorrect arguments to MpermNKL-benchmark")
        (newline))))

(define (run-benchmark-x . args)
  (apply run-r7rs-benchmark args))

(define (run-benchmark)
  (let* ([input1 (read)]
         [input2 (read)]
         [input3 (read)]
         [input4 (read)]
         [output (read)]
         [s4 (number->string input4)]
         [s3 (number->string input3)]
         [s2 (number->string input2)]
         [s1 (number->string input1)]
         [name "mperm"])
    (MpermNKL-benchmark (hide input1 input1)
                        (hide input1 input2)
                        (hide input1 input3)
                        (hide input1 input4))))

(with-input-from-file "r7rs-benchmarks/inputs/mperm.input" run-benchmark)
