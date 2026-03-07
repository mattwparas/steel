(require "steel/serde")

(define foo
  (let ([a (vector 10 20 30)])
    (lambda ()
      (vector-set! a 0 (+ (vector-ref a 0) 100))
      (vector-ref a 0))))

(define new (round-trip foo))

(new)
(new)
(new)
(new)
(new)
(assert! (equal? 610 (new)))

(define round-trip-map (round-trip map))

(assert! (equal? (map add1 (range 0 5)) (round-trip-map add1 (range 0 5))))
