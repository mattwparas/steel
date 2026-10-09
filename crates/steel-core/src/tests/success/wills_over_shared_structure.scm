;; Many registered values that are in use, all holding the one large list,
;; and one that is not in use among them.
(struct frozen (payload label))

(define executor (make-will-executor))
(define ran (box '()))
(define (note! v)
  (set-box! ran (cons (frozen-label v) (unbox ran))))
(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))
(define (drain n)
  (when (> n 0)
    (will-execute executor)
    (drain (- n 1))))

(define large (range 0 20000))
(define kept (box '()))
(define (register-kept n)
  (when (> n 0)
    (let ([v (frozen large n)])
      (set-box! kept (cons v (unbox kept)))
      (will-register executor v note!))
    (register-kept (- n 1))))
(define (register-unheld label)
  (will-register executor (frozen large label) note!)
  #t)

(register-kept 150)
(register-unheld 'unheld)
(register-kept 150)
(collect 6)
(will-execute executor)
(assert! (equal? (unbox ran) '(unheld)))

;; The list is as it was.
(assert! (= (length large) 20000))
(assert! (= (apply + large) 199990000))

;; Let go of, they are all ready.
(define (let-go!)
  (set-box! kept '())
  #t)
(let-go!)
(collect 6)
(drain 300)
(assert! (= (length (unbox ran)) 301))
(assert! (= (apply + (cdr (reverse (unbox ran)))) (* 2 (apply + (range 1 151)))))
