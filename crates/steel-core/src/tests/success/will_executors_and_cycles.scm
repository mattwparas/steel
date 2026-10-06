;; A value registered with a will executor is ready once nothing else refers to
;; it, and that includes a closure, a list or an immutable struct whose only
;; other references come from a cycle it is itself part of.
(struct node (next label) #:mutable)
(struct frozen (cell label))

(define ran (box '()))
(define executor (make-will-executor))
(define (note! label)
  (lambda (v) (set-box! ran (cons label (unbox ran)))))
(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))

;; Runs one finalizer, which has to be there, and says which it was.
(define (next-ready)
  (collect 2)
  (will-execute executor)
  (car (unbox ran)))

;; Every kind of cycle, with nothing else referring to it.
(define (through-box)
  (let* ([cell (box #f)]
         [s (frozen cell 'through-box)])
    (set-box! cell s)
    (will-register executor s (note! 'through-box))))
(define (through-struct)
  (let* ([n (node #f 'n)]
         [s (frozen n 'through-struct)])
    (set-node-next! n s)
    (will-register executor s (note! 'through-struct))))
(define (closure-through-box)
  (let* ([cell (box #f)]
         [f (lambda () (unbox cell))])
    (set-box! cell f)
    (will-register executor f (note! 'closure))))
(define (list-through-vector)
  (let* ([v (mutable-vector #f)]
         [l (list v 1 2)])
    (vector-set! v 0 l)
    (will-register executor l (note! 'list))))
(define (hash-through-box)
  (let* ([cell (box #f)]
         [h (hash 'cell cell)])
    (set-box! cell h)
    (will-register executor h (note! 'hash))))
(define (held-twice-by-its-cycle)
  (let* ([first (box #f)]
         [second (mutable-vector #f #f)]
         [s (frozen (list first second) 'twice)])
    (set-box! first s)
    (vector-set! second 0 s)
    (vector-set! second 1 (list s s))
    (will-register executor s (note! 'twice))))

(through-box)
(assert! (eq? (next-ready) 'through-box))
(through-struct)
(assert! (eq? (next-ready) 'through-struct))
(closure-through-box)
(assert! (eq? (next-ready) 'closure))
(list-through-vector)
(assert! (eq? (next-ready) 'list))
(hash-through-box)
(assert! (eq? (next-ready) 'hash))
(held-twice-by-its-cycle)
(assert! (eq? (next-ready) 'twice))

;; The finalizer is handed the value with its cycle still whole.
(define seen (box #f))
(define (checked)
  (let* ([cell (box #f)]
         [s (frozen cell 'whole)])
    (set-box! cell s)
    (will-register executor
                   s
                   (lambda (v) (set-box! seen (eq? (frozen-label (unbox (frozen-cell v))) 'whole))))))
(checked)
(collect 2)
(will-execute executor)
(assert! (unbox seen))

;; Once the finalizer has run the cycle is let go of.
(define weak-to-cell (box #f))
(define (released)
  (let* ([cell (box #f)]
         [s (frozen cell 'released)])
    (set-box! cell s)
    (set-box! weak-to-cell (make-weak-box cell))
    (will-register executor s (note! 'released))))
(released)
(assert! (eq? (next-ready) 'released))
(collect 2)
(assert! (not (weak-box-value (unbox weak-to-cell))))

;; Each of these is still in use, in a different way. They are registered
;; ahead of one that isn't, and an executor hands out the first one ready, so
;; the one that isn't in use coming out shows the others were left alone.
(define kept-directly #f)
(define kept-by-its-cell #f)
(define kept-in-a-box (box #f))
(define kept-in-a-struct (node #f 'holder))
(define kept-in-a-list '())

(define (in-use)
  (let* ([cell (box #f)]
         [s (frozen cell 'directly)])
    (set-box! cell s)
    (set! kept-directly s)
    (will-register executor s (note! 'directly)))
  ;; Only the cell is referred to, and the value through it.
  (let* ([cell (box #f)]
         [s (frozen cell 'by-its-cell)])
    (set-box! cell s)
    (set! kept-by-its-cell cell)
    (will-register executor s (note! 'by-its-cell)))
  (let* ([cell (box #f)]
         [f (lambda () (unbox cell))])
    (set-box! cell f)
    (set-box! kept-in-a-box f)
    (will-register executor f (note! 'in-a-box)))
  (let* ([v (mutable-vector #f)]
         [l (list v 1 2)])
    (vector-set! v 0 l)
    (set-node-next! kept-in-a-struct l)
    (will-register executor l (note! 'in-a-struct)))
  (let* ([cell (box #f)]
         [s (frozen cell 'in-a-list)])
    (set-box! cell s)
    (set! kept-in-a-list (list 1 s 2))
    (will-register executor s (note! 'in-a-list))))

(in-use)
(through-box)
(assert! (eq? (next-ready) 'through-box))
(through-struct)
(assert! (eq? (next-ready) 'through-struct))

;; Let go of one at a time, and each is the next to be ready. A setter hands
;; back what it replaced, and whoever runs this may keep what each form
;; returns, which would be one more reference.
(define (let-go-of! which)
  (cond
    [(eq? which 'directly) (set! kept-directly #f)]
    [(eq? which 'by-its-cell) (set! kept-by-its-cell #f)]
    [(eq? which 'in-a-box) (set-box! kept-in-a-box #f)]
    [(eq? which 'in-a-struct) (set-node-next! kept-in-a-struct #f)]
    [else (set! kept-in-a-list '())])
  #t)

(let-go-of! 'directly)
(assert! (eq? (next-ready) 'directly))
(let-go-of! 'by-its-cell)
(assert! (eq? (next-ready) 'by-its-cell))
(let-go-of! 'in-a-box)
(assert! (eq? (next-ready) 'in-a-box))
(let-go-of! 'in-a-struct)
(assert! (eq? (next-ready) 'in-a-struct))
(let-go-of! 'in-a-list)
(assert! (eq? (next-ready) 'in-a-list))

;; Two registered values in the one cycle are both ready.
(define (two-in-a-cycle)
  (let* ([cell (box #f)]
         [inner (frozen cell 'inner)]
         [outer (frozen inner 'outer)])
    (set-box! cell outer)
    (will-register executor inner (note! 'inner))
    (will-register executor outer (note! 'outer))))
(two-in-a-cycle)
(define first-of-two (next-ready))
(define second-of-two (next-ready))
(assert! (or (and (eq? first-of-two 'inner) (eq? second-of-two 'outer))
             (and (eq? first-of-two 'outer) (eq? second-of-two 'inner))))
