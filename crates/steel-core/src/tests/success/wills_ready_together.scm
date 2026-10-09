;; Registered values that nothing reaches but each other are all ready at the
;; one collection, and their wills come out in the order they were registered.
(struct frozen (next label))

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

;; Nothing is collected between the wills here, so each of them was ready
;; before the first one ran.
(define (all-ran n)
  (collect 6)
  (drain n)
  (assert! (equal? (reverse (unbox ran)) (range 0 n)))
  (set-box! ran '())
  #t)

;; Each holds the one before it.
(define (nested n)
  (let loop ([i 0] [next #f])
    (when (< i n)
      (let ([v (frozen next i)])
        (will-register executor v note!)
        (loop (+ i 1) v)))))
(nested 300)
(all-ran 300)

;; Each holds the one before it through a box.
(define (chain n)
  (let loop ([i 0] [next #f])
    (when (< i n)
      (let ([v (frozen (box next) i)])
        (will-register executor v note!)
        (loop (+ i 1) v)))))
(chain 300)
(all-ran 300)

;; A ring, where each is reached from every other.
(define (ring n)
  (let ([first (box #f)])
    (let loop ([i 0] [cell first])
      (if (= i n)
          (set-box! cell (unbox first))
          (let* ([next (box #f)]
                 [v (frozen next i)])
            (set-box! cell v)
            (will-register executor v note!)
            (loop (+ i 1) next))))
    #t))
(ring 300)
(all-ran 300)

;; Lists that share a tail, each registered, and the tail too.
(define (sharing n)
  (let ([tail (list (frozen #f 0))])
    (will-register executor (car tail) note!)
    (let loop ([i 1])
      (when (< i n)
        (let ([v (frozen (cons i tail) i)])
          (will-register executor v note!)
          (loop (+ i 1)))))))
(sharing 100)
(all-ran 100)

;; Many at once.
(define counted (box 0))
(define (register-boxes n)
  (when (> n 0)
    (will-register executor (box n) (lambda (b) (set-box! counted (+ (unbox b) (unbox counted)))))
    (register-boxes (- n 1))))
(register-boxes 20000)
(collect 2)
(drain 20000)
(assert! (= (unbox counted) 200010000))
