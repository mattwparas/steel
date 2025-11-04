;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2007 William D Clinger.
;;
;; Permission to copy this software, in whole or in part, to use this
;; software for any lawful purpose, and to redistribute this software
;; is granted subject to the restriction that all copies made of this
;; software must include this copyright notice in full.
;;
;; I also request that you send me a copy of any improvements that you
;; make to this software so that they may be incorporated within it to
;; the benefit of the Scheme community.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tests of string <-> bytevector conversions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (import (scheme base)
;         (scheme read)
;         (scheme write)
;         (scheme time))

(require "common.scm")

;; Crude test rig, just for benchmarking.

(define failed-tests '())

(define (test name actual expected)
  (unless (equal? actual expected)
    (display "******** FAILED TEST ******** ")
    (display name)
    (newline)
    (set! failed-tests (cons name failed-tests))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; We're limited to Ascii strings here because the R7RS doesn't
;; actually require anything beyond Ascii.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic sanity tests, followed by stress tests on random inputs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bytevector-u8-set! bytes-set!)
(define make-bytevector make-bytes)

(define (string-bytevector-tests *random-stress-tests* *random-stress-test-max-size*)

  (define (test-roundtrip bvec tostring tobvec)
    (let* ([s1 (tostring bvec)]
           [b2 (tobvec s1)]
           [s2 (tostring b2)])
      (test "round trip of string conversion" (string=? s1 s2) #t)))

  ;; This random number generator doesn't have to be good.
  ;; It just has to be fast.

  (define random
    (letrec ([random14 (lambda (n)
                         (set! x (remainder (+ (* a x) c) (+ m 1)))
                         (remainder (quotient x 8) n))]
             [a 701]
             [x 1]
             [c 743483]
             [m 524287]
             [loop (lambda (q r n)
                     (if (zero? q)
                         (remainder r n)
                         (loop (quotient q 16384) (+ (* 16384 r) (random14 16384)) n)))])
      (lambda (n)
        (if (< n 16384)
            (random14 n)
            (loop (quotient n 16384) (random14 16384) n)))))

  ;; Returns a random bytevector of length up to n,
  ;; with all elements less than 128.

  (define (random-bytevector n)
    (let* ([n (random n)]
           [bv (make-bytevector n)])
      (do ((i 0 (+ i 1))) ((= i n) bv) (bytevector-u8-set! bv i (random 128)))))

  ;; Returns a random bytevector of even length up to n.

  (define (random-bytevector2 n)
    (let* ([n (random n)]
           [n (if (odd? n)
                  (+ n 1)
                  n)]
           [bv (make-bytevector n)])
      (do ((i 0 (+ i 1))) ((= i n) bv) (bytevector-u8-set! bv i (random 128)))))

  ;; Returns a random bytevector of multiple-of-4 length up to n.

  (define (random-bytevector4 n)
    (let* ([n (random n)]
           [n (* 4 (round (/ n 4)))]
           [bv (make-bytevector n)])
      (do ((i 0 (+ i 1))) ((= i n) bv) (bytevector-u8-set! bv i (random 128)))))

  (test-roundtrip (random-bytevector 10) utf8->string string->utf8)

  (do ((i 0 (+ i 1)))
      ((= i *random-stress-tests*))
      (test-roundtrip (random-bytevector *random-stress-test-max-size*) utf8->string string->utf8)))

; (define (run-benchmark)
;   (let* ([count (read)]
;          [input1 (read)]
;          [input2 (read)]
;          [output (read)]
;          [s3 (number->string count)]
;          [s2 (number->string input2)]
;          [s1 (number->string input1)]
;          [name "bv2string"])
;     (run-r7rs-benchmark (string-append name ":" s1 ":" s2 ":" s3)
;                         count
;                         (lambda ()
;                           (string-bytevector-tests (hide count input1) (hide count input2))
;                           (length failed-tests))
;                         (lambda (result) (equal? result output)))))

; (with-input-from-file "r7rs-benchmarks/inputs/bv2string.input" run-benchmark)

(define (run-benchmark)
  (let* ([count 10]
         [input1 1000]
         [input2 1000]
         [output 0]
         [s3 (number->string count)]
         [s2 (number->string input2)]
         [s1 (number->string input1)]
         [name "bv2string"])
    (run-r7rs-benchmark (string-append name ":" s1 ":" s2 ":" s3)
                        count
                        (lambda ()
                          (string-bytevector-tests (hide count input1) (hide count input2))
                          (length failed-tests))
                        (lambda (result) (equal? result output)))))

(run-benchmark)
