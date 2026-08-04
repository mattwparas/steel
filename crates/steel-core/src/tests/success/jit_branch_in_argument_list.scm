;; https://github.com/mattwparas/steel/issues/680
;;
;; A branch part way through an argument list used to leave the jit's two arms
;; disagreeing about what had been moved out of, or spilled onto, the vm stack,
;; so the earlier arguments came back as #<void> or in the wrong order. The call
;; has to be indirect, so the callee isn't known at compile time.

(struct input (first second))
(struct result (first second third fourth))

(define sample-input (input 1 #t))

(define (copy-fields input-argument first-argument second-argument)
  (result first-argument
          (input-first input-argument)
          second-argument
          (and second-argument (input-second input-argument))))

(define (call-indirectly constructor)
  (constructor sample-input "argument-1" 'second-argument))

(define built (call-indirectly copy-fields))

(assert! (equal? (result-first built) "argument-1"))
(assert! (equal? (result-second built) 1))
(assert! (equal? (result-third built) 'second-argument))
(assert! (equal? (result-fourth built) #t))

;; Same shape without any structs - the callee just has to not be a closure
(define (ident x) x)

(define (build a first-argument second-argument)
  (list first-argument 1 second-argument (and second-argument (ident a))))

(define (call-builder f)
  (f 0 "argument-1" 'second-argument))

(assert! (equal? (call-builder build) (list "argument-1" 1 'second-argument 0)))
