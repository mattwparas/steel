;;; The mutable string representation used by mutable-strings.scm.
;;;
;;; Kept in its own module for the same reason as mpair-struct.scm: Steel
;;; expands `struct` into code that calls `list-ref`, which a module shadowing
;;; the list primitives cannot also define.

(provide mstring
         mstring?
         mstring-chars)

;;; Printed as the string it stands for: `list->string` hands these back, so one
;;; can reach any printer a benchmark uses, and the struct form is unreadable.
(struct mstring (chars)
  #:printer (lambda (obj printer-function)
              (printer-function (vector->string (mstring-chars obj)))))
