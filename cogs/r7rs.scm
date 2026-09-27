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

;; An unterminated datum on one port must not be completed by the next port.
(check-err? "read, unterminated list is an error"
            (read (open-input-string "(1"))
            'unreachable)
(check-equal? "read, a later port is unaffected by an earlier unterminated list"
              2
              (read (open-input-string "2)")))

(check-err? "read, unterminated list is an error, again"
            (read (open-input-string "(3 4"))
            'unreachable)
(check-err? "read, a stray closing paren is an error"
            (read (open-input-string ")"))
            'unreachable)

(check-err? "read, a trailing quote is an error"
            (read (open-input-string "'"))
            'unreachable)
(check-equal? "read, a later port is not wrapped in a leftover quote"
              42
              (read (open-input-string "42")))

;; An unterminated datum must not wedge every subsequent read.
(check-err? "read, a lone open paren is an error"
            (read (open-input-string "("))
            'unreachable)
(check-equal? "read, reads still work after an unterminated datum"
              42
              (read (open-input-string "42")))
(check-equal? "read, reads still work after an unterminated datum, compound"
              '(5 6)
              (read (open-input-string "(5 6)")))

;; Fully valid leftovers must not leak across ports either.
(check-equal? "read, takes the first datum on the port"
              1
              (read (open-input-string "1 2")))
(check-equal? "read, unconsumed leftovers do not leak to the next port"
              9
              (read (open-input-string "9")))

;; Successive reads on one port keep working.
(check-equal? "read, successive reads on one port"
              '(1 2 3 #true)
              (let ([p (open-input-string "1 2 3")])
                (list (read p) (read p) (read p) (eof-object? (read p)))))

;; Two ports read interleaved must not disturb each other.
(check-equal? "read, interleaved reads on two ports"
              '(1 10 2 20)
              (let ([a (open-input-string "1 2 3")]
                    [b (open-input-string "10 20 30")])
                (list (read a) (read b) (read a) (read b))))

;; Ports with nothing (or nothing but trivia) in them read as eof.
(check-equal? "read, empty port is eof" #true (eof-object? (read (open-input-string ""))))
(check-equal? "read, whitespace-only port is eof" #true (eof-object? (read (open-input-string "   "))))
(check-equal? "read, comment-only port is eof" #true (eof-object? (read (open-input-string "; nothing"))))

;; read-syntax-object must keep producing syntax objects on repeated reads.
(check-equal? "read-syntax-object, successive reads stay syntax objects"
              '(#true #true)
              (let ([p (open-input-string "(a b) (c d)")])
                (list (syntax? (read-syntax-object p)) (syntax? (read-syntax-object p)))))

(check-equal? "peek-char does not consume, read-port-to-string"
              '(#\a "abcdef")
              (let ([p (open-input-string "abcdef")])
                (list (peek-char p) (read-port-to-string p))))

(check-equal? "peek-char does not consume, read-port-to-bytes"
              '(#\a #u8(97 98 99 100 101 102))
              (let ([p (open-input-string "abcdef")])
                (list (peek-char p) (read-port-to-bytes p))))

;; The newline lands past the peek buffer here
(check-equal? "peek-char does not consume, read-line"
              '(#\h "hello world" "second")
              (let ([p (open-input-string "hello world\nsecond")])
                (list (peek-char p) (read-line p) (read-line p))))

;; and inside it here, which is a separate path
(check-equal? "peek-char does not consume, read-line with newline in the peek buffer"
              '(#\a "ab" "cd")
              (let ([p (open-input-string "ab\ncd")])
                (list (peek-char p) (read-line p) (read-line p))))

;; A multi-byte character can straddle the peek buffer boundary.
(check-equal? "peek-char does not consume, multi-byte characters"
              '(#\a "aλμνξ")
              (let ([p (open-input-string "aλμνξ")])
                (list (peek-char p) (read-port-to-string p))))

(check-equal? "peek-char does not consume, then read"
              '(#\( (x y))
              (let ([p (open-input-string "(x y)")])
                (list (peek-char p) (read p))))

;; Without a peek in front of them the bulk reads must be unchanged.
(check-equal? "read-port-to-string with no preceding peek"
              "abcdef"
              (read-port-to-string (open-input-string "abcdef")))
(check-equal? "read-line with no preceding peek"
              '("xy" "z")
              (let ([p (open-input-string "xy\nz")]) (list (read-line p) (read-line p))))

(check-equal? "equal?, rational inside a list" #true (equal? (list 1/2) (list 1/2)))
(check-equal? "equal?, rational inside a vector" #true (equal? (vector 1/2) (vector 1/2)))
(check-equal? "equal?, differing rationals inside a list" #false (equal? (list 1/2) (list 1/3)))

(check-equal? "equal?, complex inside a list" #true (equal? (list 1+2i) (list 1+2i)))
(check-equal? "equal?, complex with rational parts inside a list"
              #true
              (equal? (list 1/2+3/4i) (list 1/2+3/4i)))
(check-equal? "equal?, complex differing in the real part" #false (equal? (list 1+2i) (list 3+2i)))
(check-equal? "equal?, complex differing in the imaginary part" #false (equal? (list 1+2i) (list 1+3i)))

(check-equal? "equal?, bytevector inside a list" #true (equal? (list #u8(1 2)) (list #u8(1 2))))
(check-equal? "equal?, bytevector inside a vector"
              #true
              (equal? (vector #u8(1 2)) (vector #u8(1 2))))
(check-equal? "equal?, empty bytevector inside a list" #true (equal? (list #u8()) (list #u8())))
(check-equal? "equal?, bytevectors differing in contents"
              #false
              (equal? (list #u8(1 2)) (list #u8(1 3))))
(check-equal? "equal?, bytevectors differing in length"
              #false
              (equal? (list #u8(1 2)) (list #u8(1 2 3))))
(check-equal? "equal?, bytevector is not a list" #false (equal? (list #u8(1 2)) (list (list 1 2))))

(check-equal? "equal?, big rational inside a list"
              #true
              (equal? (list (/ 123456789012345678901234567890 7))
                      (list (/ 123456789012345678901234567890 7))))
(check-equal? "equal?, differing big rationals inside a list"
              #false
              (equal? (list (/ 123456789012345678901234567890 7))
                      (list (/ 123456789012345678901234567890 11))))

;; Nested more than one level deep, to make sure these go through the queue.
(check-equal? "equal?, bytevector nested two containers deep"
              #true
              (equal? (list (vector #u8(9))) (list (vector #u8(9)))))

(check-equal? "read leaves the remainder of the port readable as characters"
              '((1 2) #\space "rest")
              (let ([p (open-input-string "(1 2) rest")])
                (list (read p) (read-char p) (read-line p))))

(check-equal? "read then peek-char"
              '((1 2) #\space)
              (let ([p (open-input-string "(1 2) rest")])
                (list (read p) (peek-char p))))

(check-equal? "read-char then read"
              '(#\space (a b))
              (let ([p (open-input-string " (a b)")])
                (list (read-char p) (read p))))

(check-equal? "read consumes only its own datum, not trailing text"
              '((a) " tail")
              (let ([p (open-input-string "(a) tail")])
                (list (read p) (read-port-to-string p))))

(check-equal? "read twice, then the remainder as characters"
              '(1 2 " 3 4")
              (let ([p (open-input-string "1 2 3 4")])
                (list (read p) (read p) (read-port-to-string p))))

(check-equal? "read, a failed read leaves the rest of the port readable"
              '(a b)
              (let ([p (open-input-string ") (a b)")])
                (with-handler (lambda (err) 'ignored) (read p))
                (read p)))

;; `#!` is only a shebang at the very start of a source
(check-equal? "read, #! after a datum is an ordinary token"
              3
              (let ([p (open-input-string "(a)#!eof\n(b)")])
                (length (list (read p) (read p) (read p)))))

(check-equal? "read, a shebang line at the start of a source is skipped"
              '(a b)
              (read (open-input-string "#! /usr/bin/steel\n(a b)")))

(define r7rs-test-stats (get-test-stats))

(displayln "Passed: " (hash-ref r7rs-test-stats 'success-count))
(displayln "Skipped compilation (expected failure): " (hash-ref r7rs-test-stats 'failed-to-compile))
(displayln "Failed: " (hash-ref r7rs-test-stats 'failure-count))

(for-each (lambda (x) (displayln "    > " x)) (hash-ref r7rs-test-stats 'failures))
