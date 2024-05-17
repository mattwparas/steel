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

(provide __module__)

(define __module__ 'r5rs-test-suite)

(check-equal? "Parsing hex" #x0f 15)
(check-equal? "Parsing octal" #o0777 511)
(check-equal? "Parsing binary" #b0110 6)

(check-equal? "Empty anonymous function with variable arity" ((lambda x x)) '())

(check-equal? "filter treats lists as true" (filter (lambda (n) (list 1 2)) (list 1 2)) '(1 2))

(check-equal?
 "Symbols are interned correctly - lists should use the existing symbols that have been interned"
 (eq? 'definitely-hasnt-been-seen-before
      (let ([_ (#%black-box)]) (car '(definitely-hasnt-been-seen-before))))
 #true)

(check-equal? "addition" 8 ((lambda (x) (+ x x)) 4))

(check-equal? "Variable arity function call" '(3 4 5 6) ((lambda x x) 3 4 5 6))

(check-equal? "Rest arguments" '(5 6) ((lambda (x y . z) z) 3 4 5 6))

(check-equal? "Branching with >" 'yes (if (> 3 2) 'yes 'no))

(check-equal? "Branch with <" 'no (if (> 2 3) 'yes 'no))

(check-equal? "Numeric operations with if" 1 (if (> 3 2) (- 3 2) (+ 3 2)))

(check-equal? "Cond with >"
              'greater
              (cond
                [(> 3 2) 'greater]
                [(< 3 2) 'less]))

(check-equal? "Cond with equal"
              'equal
              (cond
                [(> 3 3) 'greater]
                [(< 3 3) 'less]
                [else 'equal]))

(check-equal? "Case macro"
              'composite
              (case (* 2 3)
                [(2 3 5 7) 'prime]
                [(1 4 6 8 9) 'composite]))

(check-equal? "Case with chars"
              'consonant
              (case (car '(c d))
                [(a e i o u) 'vowel]
                [(w y) 'semivowel]
                [else 'consonant]))

(check-equal? "and true" #t (and (= 2 2) (> 2 1)))

(check-equal? "and false" #f (and (= 2 2) (< 2 1)))

(check-equal? "and returns last in the list" '(f g) (and 1 2 'c '(f g)))

(check-equal? "and defaults to true" #t (and))

(check-equal? "or true on the first" #t (or (= 2 2) (> 2 1)))

(check-equal? "or true on the first, second not true" #t (or (= 2 2) (< 2 1)))

;; TODO
(skip-compile (check-equal? '(b c) (or (memq 'b '(a b c)) (/ 3 0))))

(check-equal? "basic let" 6 (let ([x 2] [y 3]) (* x y)))

(check-equal? "basic let with multiple levels"
              35
              (let ([x 2] [y 3]) (let ([x 7] [z (+ x y)]) (* z x))))

(check-equal? "basic let*" 70 (let ([x 2] [y 3]) (let* ([x 7] [z (+ x y)]) (* z x))))

(check-equal? "interior define"
              -2
              (let ()
                (define x 2)
                (define f (lambda () (- x)))
                (f)))

;; TODO: This gets converted into a plain let,
;; and then somehow the variable name gets overwritten? Maybe during constant propagation?
(define let*-def 1)
(let* ()
  (define let*-def 2)
  #f)
(check-equal? "Redefine top level with interior define, stays the same" 1 let*-def)

;; TODO: `do` macro
(skip-compile
 (check-equal '#(0 1 2 3 4)
              (do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i)))
 (check-equal 25
              (let ([x '(1 3 5 7 9)]) (do ((x x (cdr x)) (sum 0 (+ sum (car x)))) ((null? x) sum)))))

(check-equal? "named let"
              '((6 1 3) (-5 -2))
              (let loop ([numbers '(3 -2 1 6 -5)] [nonneg '()] [neg '()])
                (cond
                  [(null? numbers) (list nonneg neg)]
                  [(>= (car numbers) 0) (loop (cdr numbers) (cons (car numbers) nonneg) neg)]
                  [(< (car numbers) 0) (loop (cdr numbers) nonneg (cons (car numbers) neg))])))

(check-equal? "simple quasiquote and unquote" '(list 3 4) `(list ,(+ 1 2) 4))

(check-equal? "quasiquote and unquote with more" '(list a 'a) (let ([name 'a]) `(list ,name ',name)))

(check-equal? "unquote splicing" '(a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))

(check-equal? "unquote splicing inside quasiquote"
              '(a 3 `(list ,@(map abs '(4 -5 6)) b))
              `(a ,(+ 1 2) `(list ,@(map abs '(4 -5 6)) b)))

(check-equal? "unquote splicing with unquote"
              '(10 5 4 16 9 8)
              `(10 5 ,(expt 2 2) ,@(map (lambda (n) (expt n 2)) '(4 3)) 8))

(check-equal? "more complex unquote"
              '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
              `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))

(check-equal? "double unquote and quote"
              '(a `(b ,x ,'y d) e)
              (let ([name1 'x] [name2 'y]) `(a `(b ,,name1 ,',name2 d) e)))

(check-equal? "named quasiquote" '(list 3 4) (quasiquote (list (unquote (+ 1 2)) 4)))

;; TODO: Add eqv?
(skip-compile (check-equal? #t (eqv? 'a 'a))
              (check-equal #f (eqv? 'a 'b))
              (check-equal #t (eqv? '() '()))
              (check-equal #f (eqv? (cons 1 2) (cons 1 2)))
              (check-equal #f (eqv? (lambda () 1) (lambda () 2)))
              (check-equal #t (let ([p (lambda (x) x)]) (eqv? p p))))

(check-equal? "Symbols are interned" #t (eq? 'a 'a))

;; TODO: With constant evaluation, these do end up being the same thing
(check-equal? "lists don't get interned" #f (eq? (list 'a) (list 'a)))

;; Empty lists are interned
(check-equal? "Empty lists are interned" #t (eq? '() '()))

(check-equal? "functions are equal" #t (eq? car car))

(check-equal? "Local vars that are constant point to the same object" #t (let ([x '(a)]) (eq? x x)))

(check-equal? "Function objects are eq? via pointer equality" #t (let ([p (lambda (x) x)]) (eq? p p)))

(check-equal? "Value equality for interned symbols" #t (equal? 'a 'a))

(check-equal? "Value equality for interned lists" #t (equal? '(a) '(a)))

(check-equal? "Value equality for nested interned lists" #t (equal? '(a (b) c) '(a (b) c)))

(check-equal? "String equality" #t (equal? "abc" "abc"))

(check-equal? "String inequality" #f (equal? "abc" "abcd"))

(check-equal? "String inequality, first char" #f (equal? "a" "b"))

(check-equal? "Integer equality" #t (equal? 2 2))

;; TODO: Figure these comments ones out

(check-equal? "equality with float and int" #f (equal? 2.0 2))

(check-equal? "numeric equality with float and int" #t (= 1 1.0))

(check-equal? "pointer equality with int and int" #t (eq? 1 1))

(check-equal? "pointer equality with floats" #t (eq? 1.0 1.0))

(check-equal? "numeric equality with float and float" #t (= 1.0 1.0))

(skip-compile (check-equal #f (eqv? 2 2.0))
              ;; TODO: Add make-vector function
              (check-equal #t (equal? (make-vector 5 'a) (make-vector 5 'a))))

(check-equal? "max over ints" 4 (max 3 4))

(check-equal? "max with float and int" 4 (max 3.9 4))

(check-equal? "Addition binop" 7 (+ 3 4))

(check-equal? "Addition unary op" 3 (+ 3))

(check-equal? "Addition no args" 0 (+))

(check-equal? "Multiplication one arg, int" 4 (* 4))

(check-equal? "Multiplication no args, int " 1 (*))

(check-equal? "Subtraction binop" -1 (- 3 4))

(check-equal? "Subtract three args" -6 (- 3 4 5))

(check-equal? "Subtraction unary op" -3 (- 3))

(check-equal? "Subtraction, floating point and int" -1.0 (- 3.0 4))

(check-equal? "abs int" 7 (abs -7))

(check-equal? "basic string->number" 100 (string->number "100"))
(check-equal? "string->number with radix" 256 (string->number "100" 16))
(check-equal? "string->number with radix in literal" 256 (string->number "#x100"))
(check-equal? "string->number with radix in literal and explicit one" 256 (string->number "#x100" 8))
(check-equal? "string->number with large number" 100000000000000 (string->number "100000000000000"))
(check-equal? "string->number with large number and radix" 72057594037927936 (string->number "100000000000000" 16))
(check-equal? "string->number with large number and radix literal" 4398046511104 (string->number "#o100000000000000"))
(check-equal? "string->number with different base" 127 (string->number "177" 8))
(check-equal? "string->number base 2" 5 (string->number "101" 2))
(check-equal? "string->number with scientific notation" 100.0 (string->number "1e2"))

(check-equal? "basic number->string" "100" (number->string 100))
(check-equal? "number->string with different base" "100" (number->string 256 16))
(check-equal? "number->string base 16 doesn't work" "ff" (number->string 255 16))
(check-equal? "number->string base 8" "177" (number->string 127 8))
(check-equal? "number->string base 2" "101" (number->string 5 2))

;; TODO: Adjust the below check-equals

(skip-compile (check-equal 1 (modulo 13 4))
              (check-equal 1 (remainder 13 4))
              (check-equal 3 (modulo -13 4))
              (check-equal -1 (remainder -13 4))
              (check-equal -3 (modulo 13 -4))
              (check-equal 1 (remainder 13 -4))
              (check-equal -1 (modulo -13 -4))
              (check-equal -1 (remainder -13 -4))
              (check-equal 4 (gcd 32 -36))
              (check-equal 288 (lcm 32 -36)))

(check-equal? "integers are truthy" #f (not 3))

(check-equal? "lists are truthy" #f (not (list 3)))

(check-equal? "empty lists are true" #f (not '()))

(check-equal? "empty lists are true, constructor" #f (not (list)))

(check-equal? "ints are not bools" #f (boolean? 0))

(check-equal? "empty list is not a boolean" #f (boolean? '()))

; (check-equal #t (pair? '(a . b)))

(check-equal? "lists are considered pairs" #t (pair? '(a b c)))

(check-equal? "cons onto empty list" '(a) (cons 'a '()))

(check-equal? "cons list onto list" '((a) b c d) (cons '(a) '(b c d)))

(check-equal? "cons string onto list of symbols" '("a" b c) (cons "a" '(b c)))

; (check-equal '(a . 3) (cons 'a 3))

; (check-equal '((a b) . c) (cons '(a b) 'c))

(check-equal? "take the car of a list of symbols" 'a (car '(a b c)))

(check-equal? "take the car, where the car is a list" '(a) (car '((a) b c d)))

; (check-equal 1 (car '(1 . 2)))

(check-equal? "take the cdr of a list" '(b c d) (cdr '((a) b c d)))

; (check-equal 2 (cdr '(1 . 2)))

(check-equal? "Check list predicate" #t (list? '(a b c)))

(check-equal? "Empty list is a list" #t (list? '()))

; (check-equal #f (list? '(a . b)))

; (check-equal #f
;       (let ([x (list 'a)])
;         (set-cdr! x x)
;         (list? x)))

(check-equal? "List constructor" '(a 7 c) (list 'a (+ 3 4) 'c))

(check-equal? "empty list constructor" '() (list))

(check-equal? "length of a flat list" 3 (length '(a b c)))

(check-equal? "length of a non flat list" 3 (length '(a (b) (c d e))))

(check-equal? "empty list has a length of 0" 0 (length '()))

(check-equal? "append two lists" '(x y) (append '(x) '(y)))

(check-equal? "append big list to small list" '(a b c d) (append '(a) '(b c d)))

(check-equal? "append nested lists" '(a (b) (c)) (append '(a (b)) '((c))))

; (check-equal '(a b c . d) (append '(a b) '(c . d)))

;; NOTE: Improper lists are not supported, so this should fail and we should note this in
;; in the test suite
; (check-equal? "append to empty list" 'a (append '() 'a))

(check-equal? "reverse list" '(c b a) (reverse '(a b c)))

(check-equal? "reverse nested list" '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

(check-equal? "simple list-ref" 'c (list-ref '(a b c d) 2))

(skip-compile (check-equal '(a b c) (memq 'a '(a b c)))
              (check-equal '(b c) (memq 'b '(a b c)))
              (check-equal #f (memq 'a '(b c d)))
              (check-equal #f (memq (list 'a) '(b (a) c))))

(check-equal? "simple member" '((a) c) (member (list 'a) '(b (a) c)))

(skip-compile (check-equal '(101 102) (memv 101 '(100 101 102)))
              (check-equal #f (assq (list 'a) '(((a)) ((b)) ((c)))))
              (check-equal '(5 7) (assv 5 '((2 3) (5 7) (11 13)))))

(check-equal? "assoc" '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))

(check-equal? "symbol predicate" #t (symbol? 'foo))

(check-equal? "symbol predicate from constant list" #t (symbol? (car '(a b))))

(check-equal? "symbol predicate fails on string" #f (symbol? "bar"))

(check-equal? "nil symbol is symbol" #t (symbol? 'nil))

(check-equal? "empty list is not a symbol" #f (symbol? '()))

(check-equal? "symbol->string basic case" "flying-fish" (symbol->string 'flying-fish))

(check-equal? "symbol-string works" "Martin" (symbol->string 'Martin))

(check-equal? "string losslessly moves into symbol and back"
              "Malvina"
              (symbol->string (string->symbol "Malvina")))

(check-equal? "string predicate correctly identifies a string" #t (string? "a"))

(check-equal? "string predicate fails on a symbol" #f (string? 'a))

(check-equal? "empty string has a length of 0" 0 (string-length ""))

(check-equal? "string length correctly reported for standard string" 3 (string-length "abc"))

(check-equal? "string indexing into first character" #\a (string-ref "abc" 0))
(check-equal? "string indexing with multibyte characters" #\a (string-ref "λa" 1))
(check-equal? "string indexing into last character" #\c (string-ref "abc" 2))

(check-equal? "empty substring" "" (substring "abc" 0 0))
(check-equal? "empty substring at the end" "" (substring "abc" 3 3))
(check-equal? "substring without end" "bc" (substring "abc" 1))
(check-equal? "empty substring in the middle" "" (substring "abc" 1 1))
(check-equal? "substring just the first character" "a" (substring "abc" 0 1))
(check-equal? "substring a larger chunk" "bc" (substring "abc" 1 3))
(check-equal? "substring with multibyte characters" "λμ" (substring "λλμν" 1 3))
(check-equal? "full substring with multibyte characters" "千葉市" (substring "千葉市" 0 3))

(check-equal? "Basic functionality of make-string" "aaa" (make-string 3 #\a))
(check-equal? "make-string with no character" "\0\0\0" (make-string 3))
(check-equal? "make-string with zero length" "" (make-string 0))

(check-equal? "string-equality with constructor, equal" #t (string=? "a" (string #\a)))
(check-equal? "string-equality with constructor, not equal" #f (string=? "a" (string #\b)))
(check-equal? "string<, true" #t (string<? "a" "aa"))
(check-equal? "string<, false" #f (string<? "aa" "a"))
(check-equal? "string<, same strings" #f (string<? "a" "a"))
(check-equal? "string <=, true" #t (string<=? "a" "aa"))
(check-equal? "string <=, same string" #t (string<=? "a" "a"))
(check-equal? "string>, true" #t (string>? "aa" "a"))
(check-equal? "string>, false" #f (string>? "a" "aa"))
(check-equal? "string>, same strings" #f (string>? "a" "a"))
(check-equal? "string >=, true" #t (string>=? "aa" "a"))
(check-equal? "string >=, same string" #t (string>=? "a" "a"))

(check-equal? "case-insensitive string-equality with constructor, equal"
              #t
              (string-ci=? "A" (string #\a)))
(check-equal? "case-insensitive string-equality with constructor, not equal"
              #f
              (string-ci=? "A" (string #\b)))
(check-equal? "case-insensitive string<, true" #t (string-ci<? "A" "aa"))
(check-equal? "case-insensitive string<, false" #f (string-ci<? "AA" "a"))
(check-equal? "case-insensitive string<, same strings" #f (string-ci<? "A" "a"))
(check-equal? "case-insensitive string <=, true" #t (string-ci<=? "A" "aa"))
(check-equal? "case-insensitive string <=, same string" #t (string-ci<=? "A" "a"))
(check-equal? "case-insensitive string>, true" #t (string-ci>? "aa" "A"))
(check-equal? "case-insensitive string>, false" #f (string-ci>? "a" "AA"))
(check-equal? "case-insensitive string>, same strings" #f (string-ci>? "A" "a"))
(check-equal? "case-insensitive string >=, true" #t (string-ci>=? "aa" "A"))
(check-equal? "case-insensitive string >=, same string" #t (string-ci>=? "a" "A"))

(check-equal? "make-string creates single character string 'a' correctly"
              #t
              (string=? "a" (make-string 1 #\a)))
(check-equal? "make-string with character 'b' does not create string 'a'"
              #f
              (string=? "a" (make-string 1 #\b)))

(check-equal? "string-append with empty string" "abc" (string-append "abc" ""))

(check-equal? "string-append with empty string on the lhs" "abc" (string-append "" "abc"))

(check-equal? "string-append with two non empty strings" "abc" (string-append "a" "bc"))

(skip-compile (check-equal '#(0 ("Sue" "Sue") "Anna")
                           (let ([vec (vector 0 '(2 2 2 2) "Anna")])
                             (vector-set! vec 1 '("Sue" "Sue"))
                             vec))
              (check-equal '(dah dah didah) (vector->list '#(dah dah didah)))
              (check-equal '#(dididit dah) (list->vector '(dididit dah))))

(check-equal? "function correctly identified as a procedure" #t (procedure? car))

(check-equal? "symbol correctly identified as NOT as a procedure" #f (procedure? 'car))

(check-equal? "user defined function correctly identified as a procedure"
              #t
              (procedure? (lambda (x) (* x x))))

(check-equal? "quoted expression correctly identified as NOT as procedure"
              #f
              (procedure? '(lambda (x) (* x x))))

(check-equal? "basic call/cc with native predicate" #t (call-with-current-continuation procedure?))

(check-equal? "basic call/cc with user defined function"
              7
              (call-with-current-continuation (lambda (k) (+ 2 5))))

(check-equal? "more complex call/cc with user defined function"
              3
              (call-with-current-continuation (lambda (k) (+ 2 5 (k 3)))))

(check-equal? "apply with native function" 7 (apply + (list 3 4)))

(check-equal? "map with user defined function" '(b e h) (map cadr '((a b) (d e) (g h))))

(check-equal? "map with numeric op" '(1 4 27 256 3125) (map (lambda (n) (expt n n)) '(1 2 3 4 5)))

(check-equal? "map with multiple list arguments" '(5 7 9) (map + '(1 2 3) '(4 5 6)))

(check-equal? "force and delay" 3 (force (delay (+ 1 2))))

(check-equal? "force and delay with local variable"
              '(3 3)
              (let ([p (delay (+ 1 2))]) (list (force p) (force p))))

(skip-compile (check-equal '#(0 1 4 9 16)
                           (let ([v (make-vector 5)])
                             (for-each (lambda (i) (vector-set! v i (* i i))) '(0 1 2 3 4))
                             v)))

(check-equal? "using else as a variable"
              'ok
              (let ([else 1])
                (cond
                  [else 'ok]
                  [#t 'bad])))

(check-equal? "Using an arrow as a variable"
              'ok
              (let ([=> 1])
                (cond
                  [#t
                   =>
                   'ok])))

(check-equal? "Override unquote in a local context" '(,foo) (let ([unquote 1]) `(,foo)))
(check-equal? "Override unquote-splicing in a local context"
              '(,@foo)
              (let ([unquote-splicing 1]) `(,@foo)))

;; TODO: Implement let-syntax
(skip-compile (check-equal 'ok
                           (let ()
                             (let-syntax ()
                               (define internal-def 'ok))
                             internal-def))
              (check-equal 'ok
                           (let ()
                             (letrec-syntax ()
                               (define internal-def 'ok))
                             internal-def)))

(check-equal? "mutation within local function"
              '(2 1)
              ((lambda ()
                 (let ([x 1])
                   (let ([y x])
                     (set! x 2)
                     (list x y))))))

(check-equal? "multiple levels of let with mutation"
              '(2 2)
              ((lambda ()
                 (let ([x 1])
                   (set! x 2)
                   (let ([y x]) (list x y))))))

(check-equal? "local mutation"
              '(1 2)
              ((lambda ()
                 (let ([x 1])
                   (let ([y x])
                     (set! y 2)
                     (list x y))))))

(check-equal? "Multiple mutations inside local context"
              '(2 3)
              ((lambda ()
                 (let ([x 1])
                   (let ([y x])
                     (set! x 2)
                     (set! y 3)
                     (list x y))))))

; (skip-compile
(check-equal? "Dynamic wind"
              '(a b c)
              (let* ([path '()] [add (lambda (s) (set! path (cons s path)))])
                (dynamic-wind (lambda () (add 'a)) (lambda () (add 'b)) (lambda () (add 'c)))
                (reverse path)))

(check-equal? "Dynamic wind more complex"
              '(connect talk1 disconnect connect talk2 disconnect)
              (let ([path '()] [c #f])
                (let ([add (lambda (s) (set! path (cons s path)))])
                  (dynamic-wind (lambda () (add 'connect))
                                (lambda ()
                                  (add (call-with-current-continuation (lambda (c0)
                                                                         (set! c c0)
                                                                         'talk1))))
                                (lambda () (add 'disconnect)))
                  (if (< (length path) 4) (c 'talk2) (reverse path)))))

; (check-equal 2
;              (let-syntax ([foo (syntax-rules :::
;                                  []
;                                  [(foo ... args :::) (args ::: ...)])])
;                (foo 3 - 5)))
; (check-equal
;  '(5 4 1 2 3)
;  (let-syntax ([foo (syntax-rules ()
;                      [(foo args ... penultimate ultimate) (list ultimate penultimate args ...)])])
;    (foo 1 2 3 4 5)))

; )

;; -------------- Report ------------------

(require "lists/lists.scm")
(define r5rs-test-stats (get-test-stats))

(displayln "Passed: " (hash-ref r5rs-test-stats 'success-count))
(displayln "Skipped compilation (expected failure): " (hash-ref r5rs-test-stats 'failed-to-compile))
(displayln "Failed: " (hash-ref r5rs-test-stats 'failure-count))

(for-each (lambda (x) (displayln "    > " x)) (hash-ref r5rs-test-stats 'failures))
