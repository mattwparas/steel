;;; The mutable pair representation used by mutable-lists.scm.
;;;
;;; This lives in its own module on purpose: Steel expands `struct` into code
;;; that calls `list-ref`, so a module that defines a struct *and* shadows
;;; `list-ref` -- as mutable-lists.scm does -- fails to load.

(provide mpair
         mpair?
         mpair-mcar
         mpair-mcdr
         set-mpair-mcar!
         set-mpair-mcdr!)

(struct mpair (mcar mcdr) #:mutable)
