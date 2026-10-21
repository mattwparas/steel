;; A builtin that calls back into Steel holds its arguments while the code it
;; calls runs. A collection then has to treat them as reachable, even when
;; nothing else refers to them.

(define (churn)
  (#%gc-collect)
  (let loop ([i 0]
             [acc '()])
    (if (= i 2000)
        (length acc)
        (loop (+ i 1) (cons (mutable-vector i i) (cons (box i) acc)))))
  (#%gc-collect))

(define (check n)
  (when (> n 0)
    (assert! (equal? (#%prim.transduce (mutable-vector (box 1) (box 2) (box 3))
                                       (mapping (lambda (b)
                                                  (churn)
                                                  (unbox b)))
                                       (into-list))
                     '(1 2 3)))
    (assert! (equal? (#%prim.transduce (list 1 2 3)
                                       (zipping (list (box 'a) (box 'b) (box 'c)))
                                       (mapping (lambda (pair)
                                                  (churn)
                                                  (unbox (cadr pair))))
                                       (into-list))
                     '(a b c)))
    (assert! (eq? (unbox (#%prim.transduce (list 1 2 3)
                                           (into-reducer (lambda (acc x)
                                                           (churn)
                                                           acc)
                                                         (box 'kept))))
                  'kept))
    (check (- n 1))))

(check 10)
