;; A value swapped out of a cell while other threads are around is kept for a
;; while in case one of them is reading it. It still has to be let go of, and
;; so does a cycle of structs nothing refers to any more.
(struct node (next payload) #:mutable)

(define stop (box #f))
(define (idle n)
  (if (unbox stop) n (idle (+ n 1))))
(define bystander (spawn-native-thread (lambda () (idle 0))))

(define shared-box (box #f))
(define shared-vector (mutable-vector #f))
(define shared-struct (node #f #f))

(define (weak-to-swapped-out store!)
  (let ([weak (make-weak-box (node #f (list 1 2 3)))])
    (store! (weak-box-value weak))
    (store! #f)
    weak))

(define from-box (weak-to-swapped-out (lambda (v) (set-box! shared-box v))))
(define from-vector (weak-to-swapped-out (lambda (v) (vector-set! shared-vector 0 v))))
(define from-struct (weak-to-swapped-out (lambda (v) (set-node-payload! shared-struct v))))

(define (weak-to-cycle)
  (let* ([a (node #f 'a)]
         [b (node a 'b)])
    (set-node-next! a b)
    (make-weak-box a)))
(define from-cycle (weak-to-cycle))

;; One that is still referred to, to show the checks below can tell.
(define kept (node #f (list 4 5 6)))
(define from-kept (make-weak-box kept))

;; Enough writes for what was put aside to be handed on and dropped.
(define (churn n)
  (when (> n 0)
    (set-box! shared-box (list n))
    (churn (- n 1))))
(churn 20000)
(set-box! shared-box #f)

(#%gc-collect)
(#%gc-collect)

(assert! (not (weak-box-value from-box)))
(assert! (not (weak-box-value from-vector)))
(assert! (not (weak-box-value from-struct)))
(assert! (not (weak-box-value from-cycle)))
(assert! (node? (weak-box-value from-kept)))

(set-box! stop #t)
(assert! (> (thread-join! bystander) 0))
