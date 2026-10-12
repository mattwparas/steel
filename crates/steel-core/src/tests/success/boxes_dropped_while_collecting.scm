(define stop (box #f))

(define (collect n)
  (if (unbox stop)
      n
      (begin
        (#%gc-collect)
        (collect (+ n 1)))))

(define collector (spawn-native-thread (lambda () (collect 0))))

(define (through-a-list n)
  (equal? (car (unbox (car (list (box (list n)) n)))) n))

(define (through-two-boxes n)
  (equal? (unbox (unbox (car (list (box (box n)) (box n))))) n))

(define (through-a-vector n)
  (equal? (vector-ref (unbox (car (list (box (mutable-vector n)) n))) 0) n))

(define (count-wrong check from to wrong)
  (if (= from to)
      wrong
      (count-wrong check (+ from 1) to (if (check from) wrong (+ wrong 1)))))

(define wrong
  (+ (count-wrong through-a-list 0 12000 0)
     (count-wrong through-two-boxes 0 6000 0)
     (count-wrong through-a-vector 0 6000 0)))

(set-box! stop #t)
(thread-join! collector)
(assert! (= wrong 0))
