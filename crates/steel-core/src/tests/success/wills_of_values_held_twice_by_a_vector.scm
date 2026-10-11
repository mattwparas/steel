;; A value in a cycle of garbage is ready however many times the cycle holds
;; it. A mutable vector here holds the same list two and three times over.

(require-builtin steel/time)

(struct frozen (payload label))

(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))

(define executor (make-will-executor))
(define ran (box '()))
(define (note value)
  (set-box! ran (cons (frozen-label value) (unbox ran))))

(define running (spawn-native-thread (lambda ()
                                       (let loop ()
                                         (will-execute executor)
                                         (loop)))))

(define (register-a-cycle label copies)
  (let* ([holder (mutable-vector)]
         [value (frozen holder label)]
         [through (list value)])
    (let loop ([i 0])
      (when (< i copies)
        (vector-push! holder through)
        (loop (+ i 1))))
    (will-register executor value note))
  #t)

(register-a-cycle 'once 1)
(register-a-cycle 'twice 2)
(register-a-cycle 'thrice 3)

(define (wait-for-all tries)
  (collect 3)
  (time/sleep-ms 20)
  (when (and (< (length (unbox ran)) 3) (> tries 0))
    (wait-for-all (- tries 1))))
(wait-for-all 100)

(assert! (= (length (unbox ran)) 3))
(assert! (equal? (sort (map symbol->string (unbox ran)) string<?) '("once" "thrice" "twice")))
