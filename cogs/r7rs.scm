;; Adapted from https://github.com/ashinn/chibi-scheme/blob/master/tests/r5rs-tests.scm
;; with the following copyright notice:
;;
;; -------------------------------------------------------------------
;;
;; Copyright (c) 2009-2021 Alex Shinn
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; -------------------------------------------------------------------
;;
;; Modified by Matt Paras for use within the Steel test suite

(require "tests/unit-test.scm"
         (for-syntax "tests/unit-test.scm"))

(set-test-mode!)

(require "lists/lists.scm")

;;;; Parameters

(define location (make-parameter "here"))

(check-equal? "Simple parameterize"
              "there"
              (parameterize ([location "there"])
                (location))) ;; "there"

(check-equal? "parameter keeps original value" "here" (location)) ;; "here"

(check-equal? "Parameter changes multiple times"
              (list "in a house" "with a mouse" "in a house")
              (parameterize ([location "in a house"])
                (list (location)
                      (parameterize ([location "with a mouse"])
                        (location))
                      (location)))) ;; '("in a house" "with a mouse" "in a house")

(check-equal? "parameter keeps original value after" "here" (location)) ;; "here"

(define (would-you-could-you?)
  (and (not (equal? (location) "here")) (not (equal? (location) "there"))))

(check-equal? "Parameters refer to the same location" #false (would-you-could-you?))

(check-equal? "Parameters refer to the same location, changed to be the same"
              #true
              (parameterize ([location "on a bus"])
                (would-you-could-you?)))

;; Bytevectors

(define bytevector? bytes?)
(define make-bytevector make-bytes)
(define bytevector-length bytes-length)
(define bytevector-u8-ref bytes-ref)
(define bytevector-append bytes-append)

(check-equal? "bytevector?, empty" #t (bytevector? #u8()))
(check-equal? "bytevector?" #t (bytevector? #u8(0 1 2)))
(check-equal? "bytevector?, false, empty vector" #f (bytevector? #()))
(check-equal? "bytevector?, false, vector" #f (bytevector? #(0 1 2)))
(check-equal? "bytevector?, false, empty list" #f (bytevector? '()))
(check-equal? "bytevector?, make-bytevector" #t (bytevector? (make-bytevector 0)))

(check-equal? "bytevector-length, empty" 0 (bytevector-length (make-bytevector 0)))
(check-equal? "bytevector-length, with zeroes" 1024 (bytevector-length (make-bytevector 1024)))
(check-equal? "bytevector-length, non-zero" 1024 (bytevector-length (make-bytevector 1024 255)))

(check-equal? "bytevector-length" 3 (bytevector-length (bytevector 0 1 2)))

(check-equal? "bytevector-u8-ref, 0" 0 (bytevector-u8-ref (bytevector 0 1 2) 0))
(check-equal? "bytevector-u8-ref, 1" 1 (bytevector-u8-ref (bytevector 0 1 2) 1))
(check-equal? "bytevector-u8-ref, 2" 2 (bytevector-u8-ref (bytevector 0 1 2) 2))

(skip-compile (check-equal? "TODO" #u8(0 255 2)
    (let ((bv (bytevector 0 1 2))) (bytevector-u8-set! bv 1 255) bv)))

(check-equal? "bytevector-copy, empty" #u8() (bytevector-copy #u8()))
(check-equal? "bytevector-copy" #u8(0 1 2) (bytevector-copy #u8(0 1 2)))
(check-equal? "bytevector-copy, with start" #u8(1 2) (bytevector-copy #u8(0 1 2) 1))
(check-equal? "bytevector-copy, with start and end" #u8(1) (bytevector-copy #u8(0 1 2) 1 2))

(skip-compile (check-equal? "TODO" #u8(1 6 7 4 5)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 1 #u8(6 7 8 9 10) 0 2)
      bv)))
(skip-compile (check-equal? "TODO" #u8(6 7 8 9 10)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 0 #u8(6 7 8 9 10))
      bv)))
(skip-compile (check-equal? "TODO" #u8(8 9 10 4 5)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 0 #u8(6 7 8 9 10) 2)
      bv)))
(skip-compile (check-equal? "TODO" #u8(1 2 6 7 8)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 2 #u8(6 7 8 9 10) 0 3)
      bv)))
(skip-compile (check-equal? "TODO" #u8(1 2 8 4 5)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 2 #u8(6 7 8 9 10) 2 3)
      bv)))

;; same source and dest
(skip-compile (check-equal? "TODO" #u8(1 1 2 4 5)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 1 bv 0 2)
      bv)))
(skip-compile (check-equal? "TODO" #u8(1 2 3 1 2)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 3 bv 0 2)
      bv)))

(check-equal? "bytevector-append, empty" #u8() (bytevector-append #u8()))
(check-equal? "bytevector-append, empties" #u8() (bytevector-append #u8() #u8()))
(check-equal? "bytevector-append, empty plus non-empty" #u8(0 1 2) (bytevector-append #u8() #u8(0 1 2)))
(check-equal? "bytevector-append, non-empty plus empty" #u8(0 1 2) (bytevector-append #u8(0 1 2) #u8()))
(check-equal? "bytevector-append" #u8(0 1 2 3 4) (bytevector-append #u8(0 1 2) #u8(3 4)))
(check-equal? "bytevector-append, multiple" #u8(0 1 2 3 4 5) (bytevector-append #u8(0 1 2) #u8(3 4) #u8(5)))

(check-equal? "utf8->string" "ABC" (utf8->string #u8(#x41 #x42 #x43)))
(check-equal? "utf8->string, multi-byte char" "λ" (utf8->string #u8(#xCE #xBB)))
(check-equal? "utf8->string with start" "ABC" (utf8->string #u8(0 #x41 #x42 #x43) 1))
(check-equal? "utf8->string with start and end" "ABC" (utf8->string #u8(0 #x41  #x42 #x43 0) 1 4))
(check-equal? "utf8->string with start and end, multi-byte char" "λ" (utf8->string #u8(0 #xCE #xBB 0) 1 3))
(check-equal? "string->utf8" #u8(#x41 #x42 #x43) (string->utf8 "ABC"))
(check-equal? "string->utf8 with start" #u8(#x42 #x43) (string->utf8 "ABC" 1))
(check-equal? "string->utf8 with start and end" #u8(#x42) (string->utf8 "ABC" 1 2))
(check-equal? "string->utf8 with start and end, multi-byte" #u8(#xCE #xBB) (string->utf8 "σλC" 1 2))
(check-equal? "string->utf8, multi-byte char" #u8(#xCE #xBB) (string->utf8 "λ"))

(check-equal? "char->integer, special escape, null" 0 (char->integer (read (open-input-string "#\\null"))))
(check-equal? "char->integer, special escape, alarm" 7 (char->integer (read (open-input-string "#\\alarm"))))
(check-equal? "char->integer, special escape, backspace" 8 (char->integer (read (open-input-string "#\\backspace"))))
(check-equal? "char->integer, special escape, tab" 9 (char->integer (read (open-input-string "#\\tab"))))
(check-equal? "char->integer, special escape, newline" 10 (char->integer (read (open-input-string "#\\newline"))))
(check-equal? "char->integer, special escape, return" 13 (char->integer (read (open-input-string "#\\return"))))
(check-equal? "char->integer, special escape, delete" #x7F (char->integer (read (open-input-string "#\\delete"))))
(check-equal? "char->integer, special escape, escape" #x1B (char->integer (read (open-input-string "#\\escape"))))
(check-equal? "char->integer, multi-byte" #x03BB (char->integer (read (open-input-string "#\\λ"))))

(define any-arity
  (case-lambda
    (() 'zero)
    ((x) x)
    ((x y) (cons x y))
    ((x y z) (list x y z))
    (args (cons 'many args))))

(check-equal? "case-lambda, any arity, 0 args" (any-arity) 'zero)
(check-equal? "case-lambda, any arity, 1 args" (any-arity 1) 1)
(check-equal? "case-lambda, any arity, 2 args" (any-arity 1 2) '(1 . 2))
(check-equal? "case-lambda, any arity, 3 args" (any-arity 1 2 3) '(1 2 3))
(check-equal? "case-lambda, any arity, 4 args" (any-arity 1 2 3 4) '(many 1 2 3 4))

(define rest-arity
  (case-lambda
    (() '(zero))
    ((x) (list 'one x))
    ((x y) (list 'two x y))
    ((x y . z) (list 'more x y z))))

(check-equal? "case-lambda, rest arity, 0 args" (rest-arity) '(zero))
(check-equal? "case-lambda, rest arity, 1 args" (rest-arity 1) '(one 1))
(check-equal? "case-lambda, rest arity, 2 args" (rest-arity 1 2) '(two 1 2))
(check-equal? "case-lambda, rest arity, 3 args" (rest-arity 1 2 3) '(more 1 2 (3)))

(define dead-clause
  (case-lambda
    ((x . y) 'many)
    (() 'none)
    (foo 'unreachable)))

(check-equal? "case-lambda, dead clause, 0 args" (dead-clause) 'none)
(check-equal? "case-lambda, dead clause, 1 args" (dead-clause 1) 'many)
(check-equal? "case-lambda, dead clause, 2 args" (dead-clause 1 2) 'many)
(check-equal? "case-lambda, dead clause, 3 args" (dead-clause 1 2 3) 'many)

(define r7rs-test-stats (get-test-stats))

(displayln "Passed: " (hash-ref r7rs-test-stats 'success-count))
(displayln "Skipped compilation (expected failure): " (hash-ref r7rs-test-stats 'failed-to-compile))
(displayln "Failed: " (hash-ref r7rs-test-stats 'failure-count))

(for-each (lambda (x) (displayln "    > " x)) (hash-ref r7rs-test-stats 'failures))
