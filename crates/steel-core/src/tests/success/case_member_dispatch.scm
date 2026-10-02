;; A `case` clause with several datums tests its key with `#%case-member?`,
;; which the jit lowers to a bitmap test for chars and fixnums below 128 and
;; to compares for the rest. Run hot so the jitted versions are the ones
;; checked.
(define (classify x)
  (case x
    [(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w
      #\x #\y #\z)
     'lower]
    [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) 'digit]
    [(#\space #\newline #\tab) 'space]
    [(#\λ #\é #\~) 'mixed-char]
    [(0 1 2 63 64 65 127) 'small-int]
    [(128 -1 1000000) 'big-int]
    [(foo bar baz) 'sym]
    [(#t) 'true]
    [(#f #\A) 'false-or-A]
    [() 'never]
    [(s1 s2 s3 s4 s5 s6 s7 s8 s9 s10) 'many-syms]
    [else 'other]))

(define inputs
  (list #\a #\m #\z #\0 #\9 #\space #\newline #\tab #\λ #\é #\~ 0 1 63 64 65 127 128 -1 1000000 'foo
        'baz 's1 's10 #t #f #\A #\B #\{ #\` 66 3 "a" 1.0 '() 'qux (string->symbol "bar")))
(define expected
  '(lower lower lower digit digit space space space mixed-char mixed-char mixed-char small-int small-int
    small-int small-int small-int small-int big-int big-int big-int sym sym many-syms many-syms true
    false-or-A false-or-A other other other other other other other other other sym))

(define (loop n)
  (when (> n 0)
    (map classify inputs)
    (loop (- n 1))))
(loop 2000)
(assert! (equal? (map classify inputs) expected))
