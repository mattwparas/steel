;;; Output primitives that have no `#%prim.` form, captured here -- in a module
;;; that does not shadow them -- for mutable-strings.scm to delegate to.

(provide native-display
         native-write
         native-write-string)

(define (native-display . args)
  (apply display args))

(define (native-write . args)
  (apply write args))

(define (native-write-string . args)
  (apply write-string args))
