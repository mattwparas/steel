;; An executor holds its will procedures, so a value one of them reaches is
;; in use and not ready, whichever value the procedure is the will of.
(struct frozen (cell label))

(define executor (make-will-executor))
(define ran (box '()))
(define (note! label)
  (lambda (v) (set-box! ran (cons label (unbox ran)))))
(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))

(define keep (box #f))
(define (let-go!)
  (set-box! keep #f)
  #t)

;; An executor hands out wills in the order they became ready. So a will
;; registered after `v` coming out first shows that `v` was not ready.
(define (register-unheld label)
  (will-register executor (list label) (note! label))
  #t)
(define (next-ready)
  (collect 6)
  (will-execute executor)
  (car (unbox ran)))

;; `v` is in a cycle with a list, and the will of `w` reaches the list.
(define (reached-through-its-cycle)
  (let* ([cell (box #f)]
         [v (frozen cell 'v)]
         [members (list v)]
         [w (list 1 2 3)])
    (set-box! cell members)
    (set-box! keep w)
    (will-register executor v (note! 'v))
    (will-register executor
                   w
                   (lambda (x)
                     (set-box! ran (cons (list 'w 'reaches (frozen-label (car members))) (unbox ran))))))
  #t)

(reached-through-its-cycle)
(register-unheld 'after-cycle)
(assert! (eq? (next-ready) 'after-cycle))

;; With `w` let go of, its will runs, and still finds `v` whole.
(let-go!)
(assert! (equal? (next-ready) '(w reaches v)))
;; Only then is `v` ready.
(assert! (eq? (next-ready) 'v))

;; The same with no cycle. The will of `w` refers to `v` itself.
(define (reached-directly)
  (let* ([v (frozen 1 'direct)]
         [w (list 1 2 3)])
    (set-box! keep w)
    (will-register executor v (note! 'direct))
    (will-register executor
                   w
                   (lambda (x) (set-box! ran (cons (list 'w 'reaches (frozen-label v)) (unbox ran))))))
  #t)

(reached-directly)
(register-unheld 'after-direct)
(assert! (eq? (next-ready) 'after-direct))
(let-go!)
(assert! (equal? (next-ready) '(w reaches direct)))
(assert! (eq? (next-ready) 'direct))

;; And with a box, which a mark answers for.
(define (box-reached)
  (let* ([v (box 'boxed)]
         [w (list 1 2 3)])
    (set-box! keep w)
    (will-register executor v (note! 'boxed))
    (will-register executor w (lambda (x) (set-box! ran (cons (list 'w 'reaches (unbox v)) (unbox ran))))))
  #t)

(box-reached)
(register-unheld 'after-box)
(assert! (eq? (next-ready) 'after-box))
(let-go!)
(assert! (equal? (next-ready) '(w reaches boxed)))
(assert! (eq? (next-ready) 'boxed))

;; A value that is ready and has not been handed out is held too, and so is
;; what it reaches.
(define inner-kept (box #f))
(define (register-outer)
  (let* ([inner (frozen 1 'inner)]
         [outer (frozen inner 'outer)])
    (set-box! inner-kept inner)
    (will-register executor outer (note! 'outer)))
  #t)
(define (register-inner)
  (will-register executor (unbox inner-kept) (note! 'inner))
  (set-box! inner-kept #f)
  #t)

(set-box! ran '())
(register-outer)
(collect 6)
(register-inner)
(register-unheld 'after-inner)
(collect 6)
(will-execute executor)
(will-execute executor)
(assert! (equal? (unbox ran) '(after-inner outer)))
(assert! (eq? (next-ready) 'inner))
