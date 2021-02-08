;; Implementation of a stack in `steel`
;; Returning multiple values requires returning as a list
;; Functional data structure, is definitely verbose but is super helpful
;; Maybe look into using monadic forms in order to not have the verbosity
;; associated with a functional data structure


;; ---------------------------------------------------------------------
;; Implementation of a stack purely using a list
;; ---------------------------------------------------------------------


;; destruct works like so:
;; (destruct (a b c) value)
;;  ...
;; (define a (car value))
;; (define b (car (cdr value)))
;; (define c (car (cdr (cdr value))))
(define-syntax destruct
  (syntax-rules ()
    [(destruct (var) ret-value)
     (define (datum->syntax var) (car ret-value))]
    [(destruct (var1 var2 ...) ret-value)
     (begin (define (datum->syntax var1) (car ret-value))
            (destruct (var2 ...) (cdr ret-value)))]))


(define (make-stack) '())

;; stack -> '(value, stack)
(define (pop stack)
  (if (null? stack)
      '(#f '())
      (list (car stack) (cdr stack))))

;; value -> stack -> stack
(define push cons)

;; instantiate an empty stack
(define my-stack (make-stack))

;; Bind the last few values from the stack
;; Push the values 1, 2, 3, 4, then pop and return the value and the stack
(destruct (pop-val new-stack)
          (->> my-stack
               (push 1)
               (push 2)
               (push 3)
               (push 4)
               (pop)))

pop-val ;; => 4
new-stack ;; => '(3 2 1)
