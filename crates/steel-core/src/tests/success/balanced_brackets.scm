(define (balanced-brackets string)
  (define (b chars sum)
    (cond
      [(< sum 0) #f]
      [(and (null? chars) (= 0 sum)) #t]
      [(null? chars) #f]
      [(equal? #\[ (car chars)) (b (cdr chars) (+ sum 1))]
      [(equal? #\] (car chars)) (b (cdr chars) (- sum 1))]
      [else #f]))
  (b (string->list string) 0))

(assert! (balanced-brackets ""))

(assert! (balanced-brackets "[]"))
(assert! (balanced-brackets "[][]"))
(assert! (balanced-brackets "[[][]]"))

(assert! (not (balanced-brackets "][")))
(assert! (not (balanced-brackets "][][")))
(assert! (not (balanced-brackets "[]][[]")))
