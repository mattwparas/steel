;; `case` compares with `eqv?`, which the jit inlines against a constant char,
;; fixnum, boolean or symbol. Run hot so the jitted versions are the ones
;; checked.
(define (classify c)
  (case c
    [(#\a #\e #\i #\o #\u) 'vowel]
    [(#\space) 'space]
    [(1 2 3) 'small]
    [(42) 'answer]
    [(foo bar) 'sym]
    [(#t) 'true]
    [(#f) 'false]
    [else 'other]))

(define inputs (list #\a #\u #\space #\b 1 3 4 42 'foo 'bar 'baz #t #f "a" 1.0 '()))
(define expected
  '(vowel vowel space other small small other answer sym sym other true false other other other))

(define (loop-classify n)
  (when (> n 0)
    (map classify inputs)
    (loop-classify (- n 1))))
(loop-classify 1000)
(assert! (equal? (map classify inputs) expected))

;; `eqv?` is not `equal?`: equal strings that are distinct objects don't match.
(define (case-string s)
  (case s
    [("abc") 'matched]
    [else 'no]))
(assert! (equal? (case-string (string-append "ab" "c")) 'no))

;; `char-whitespace?` answers ASCII inline and everything else in the primitive.
(define chars
  (list #\space #\tab #\newline #\return #\a #\0 (integer->char 11) (integer->char 12)
        (integer->char 8) (integer->char 14) (integer->char #xA0) (integer->char #x2003) #\~))
(define (ws c) (char-whitespace? c))
(define (loop-ws n)
  (when (> n 0)
    (map ws chars)
    (loop-ws (- n 1))))
(loop-ws 1000)
(assert! (equal? (map ws chars) '(#t #t #t #t #f #f #t #t #f #f #t #t #f)))
(assert! (equal? (with-handler (lambda (e) 'err) (ws 5)) 'err))

;; Builtins in tail position: one that produces a value stays in compiled code,
;; ones that take or replace a frame go back to the interpreter.
(define (sym s) (string->symbol s))
(define (ap f xs) (apply f xs))
(define (k-tail x) (call/cc (lambda (k) (if (> x 0) (k x) 'neg))))
(define (sum . xs) (if (null? xs) 0 (+ (car xs) (apply sum (cdr xs)))))
(define (loop-tails n acc)
  (if (= n 0)
      acc
      (begin
        (sym "abc")
        (k-tail n)
        (loop-tails (- n 1) (+ acc (ap sum (list 1 2 3)))))))
(assert! (equal? (loop-tails 1000 0) 6000))
(assert! (eq? (sym "hello") 'hello))
(assert! (equal? (ap list '(1 2 3)) '(1 2 3)))
(assert! (equal? (list (k-tail 5) (k-tail -1)) '(5 neg)))
(assert! (equal? (with-handler (lambda (e) 'err) (sym 5)) 'err))

;; `char=?` compares code points inline for chars; anything else raises in the
;; primitive.
(define (nl? c) (char=? c #\newline))
(define (same a b) (char=? a b))
(define some-chars (list #\a #\newline #\b (integer->char #x2003) #\~))
(define (loop-chars n)
  (when (> n 0)
    (map nl? some-chars)
    (map same some-chars some-chars)
    (loop-chars (- n 1))))
(loop-chars 1000)
(assert! (equal? (map nl? some-chars) '(#f #t #f #f #f)))
(assert! (equal? (map same some-chars (reverse some-chars)) '(#f #f #t #f #f)))
(assert! (equal? (with-handler (lambda (e) 'err) (nl? 5)) 'err))
(assert! (equal? (with-handler (lambda (e) 'err) (same #\a "a")) 'err))
