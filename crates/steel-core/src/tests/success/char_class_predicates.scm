;; The char class predicates, run hot so the jitted versions are checked: an
;; ascii char is classified inline, anything else by the primitive.
(define chars
  (list #\a #\z #\A #\Z #\m #\0 #\9 #\5 #\space #\tab #\newline #\return #\@ #\[ #\` #\{ #\/ #\:
        #\λ #\Λ #\é #\É #\٣ #\越 #\x00A0 #\_))

(define (classes c)
  (list (char-alphabetic? c)
        (char-numeric? c)
        (char-upper-case? c)
        (char-lower-case? c)
        (char-whitespace? c)))

(define expected
  '((#t #f #f #t #f) (#t #f #f #t #f) (#t #f #t #f #f) (#t #f #t #f #f) (#t #f #f #t #f)
    (#f #t #f #f #f) (#f #t #f #f #f) (#f #t #f #f #f) (#f #f #f #f #t) (#f #f #f #f #t)
    (#f #f #f #f #t) (#f #f #f #f #t) (#f #f #f #f #f) (#f #f #f #f #f) (#f #f #f #f #f)
    (#f #f #f #f #f) (#f #f #f #f #f) (#f #f #f #f #f) (#t #f #f #t #f) (#t #f #t #f #f)
    (#t #f #f #t #f) (#t #f #t #f #f) (#f #t #f #f #f) (#t #f #f #f #f) (#f #f #f #f #t)
    (#f #f #f #f #f)))

(define (loop n acc)
  (if (= n 0)
      acc
      (loop (- n 1) (map classes chars))))
(assert! (equal? (loop 2000 '()) expected))

;; A non-char raises, from the jitted code as well.
(define (numeric-or-raised x)
  (with-handler (lambda (e) 'raised) (char-numeric? x)))
(define (loop-raise n acc)
  (if (= n 0)
      acc
      (loop-raise (- n 1) (list (numeric-or-raised #\1) (numeric-or-raised 1) (numeric-or-raised "1")))))
(assert! (equal? (loop-raise 2000 '()) '(#t raised raised)))
