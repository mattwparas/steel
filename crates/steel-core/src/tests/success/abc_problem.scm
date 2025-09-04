(define *blocks*
  '((#\B #\O) (#\X #\K) (#\D #\Q) (#\C #\P) (#\N #\A)
    (#\G #\T) (#\R #\E) (#\T #\G) (#\Q #\D) (#\F #\S)
    (#\J #\W) (#\H #\U) (#\V #\I) (#\A #\N) (#\O #\B)
    (#\E #\R) (#\F #\S) (#\L #\Y) (#\P #\C) (#\Z #\M)))

(define (exists p? li)
  (and (not (null? li))
       (or (p? (car li))
           (exists p? (cdr li)))))

(define (remove-one x li)
  (cond
    ((null? li) '())
    ((equal? (car li) x) (cdr li))
    (else (cons (car li) (remove-one x (cdr li))))))

(define (can-make-list? li blocks)
  (or (null? li)
      (exists
       (lambda (block)
         (and
          (member (char-upcase (car li)) block)
          (can-make-list? (cdr li) (remove-one block blocks))))
       blocks)))

(define (can-make-word? word)
  (can-make-list? (string->list word) *blocks*))
 
 
(define *words*
  '("A" "Bark" "book" "TrEaT" "COMMON" "squaD" "CONFUSE"))

(define can-make-words
    '("A" "Bark" "TrEaT" "squaD" "CONFUSE"))

(assert! (equal? (filter can-make-word? *words*)))

; (for-each
;  (lambda (word)
;    (display (if (can-make-word? word)
;                 "   Can make word: "
;                 "Cannot make word: "))
;    (display word)
;    (newline))
;  *words*)
