;; One thread registers reference counted values with a will executor, each
;; of them twice, one runs the wills and this one collects. Every will runs,
;; and is handed its value whole.
(struct frozen (payload label))

(define executor (make-will-executor))
(define wrong (box 0))
(define ran (box 0))
(define (will value)
  (set-box! ran (+ 1 (unbox ran)))
  (unless (and (frozen? value)
               (int? (frozen-label value))
               (equal? (frozen-payload value) (list (frozen-label value) "held")))
    (set-box! wrong (+ 1 (unbox wrong)))))

(define (execute)
  (will-execute executor)
  (execute))
(define executing (spawn-native-thread execute))

(define (spin n)
  (when (> n 0)
    (spin (- n 1))))
(define (register i)
  (when (< i 5000)
    (let ([value (frozen (list i "held") i)])
      (will-register executor value will)
      (will-register executor value will))
    (spin 40)
    (register (+ i 1))))
(define registering (spawn-native-thread (lambda () (register 0))))

(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))
(collect 300)
(thread-join! registering)

;; A value becomes ready at a collection, so collect until they all have.
(define (settle tries)
  (when (and (> tries 0) (< (unbox ran) 10000))
    (#%gc-collect)
    (spin 20000)
    (settle (- tries 1))))
(settle 2000)
(assert! (= (unbox ran) 10000))
(assert! (= (unbox wrong) 0))
