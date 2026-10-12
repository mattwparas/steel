(define executor (make-will-executor))

;; Held by a cycle through a box that only a collection frees
(define (register-in-a-cycle)
  (let ([value (list 1 2)]
        [cell (box #f)])
    (set-box! cell (list cell value))
    (will-register executor value (lambda (v) 'in-a-cycle))
    #t))
(register-in-a-cycle)
(assert! (eq? 'in-a-cycle (will-execute executor)))

;; Two of them, one behind the other
(define (register-two-deep)
  (let ([value (list 3 4)]
        [inner (box #f)]
        [outer (box #f)])
    (set-box! inner (list inner value))
    (set-box! outer (list outer inner))
    (will-register executor value (lambda (v) 'two-deep))
    #t))
(register-two-deep)
(assert! (eq? 'two-deep (will-execute executor)))

;; Held by a list that only a weak box holds
(define (register-weakly-held)
  (let ([value (list 5 6)])
    (will-register executor value (lambda (v) 'weakly-held))
    (make-weak-box (list value))))
(define weak (register-weakly-held))
(assert! (eq? 'weakly-held (will-execute executor)))
(assert! (not (weak-box-value weak)))

;; A weak box whose value is still held keeps it through collections
(define kept (list 7 8))
(define weak-kept (make-weak-box kept))
(define weak-cell (make-weak-box (box kept)))
(#%gc-collect)
(#%gc-collect)
(assert! (eq? kept (weak-box-value weak-kept)))
(assert! (not (weak-box-value weak-cell)))
