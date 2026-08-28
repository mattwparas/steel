;;; Steel has no `#%prim.read`, so the builtin is captured here -- in a module
;;; that does not shadow `read` -- for mutable-lists.scm to delegate to.

(provide native-read)

(define (native-read . port)
  (apply read port))
