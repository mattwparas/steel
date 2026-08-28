;;; The mutable string representation used by mutable-strings.scm.
;;;
;;; Kept in its own module for the same reason as mpair-struct.scm: Steel
;;; expands `struct` into code that calls `list-ref`, which a module shadowing
;;; the list primitives cannot also define.

(provide mstring
         mstring?
         mstring-chars)

(struct mstring (chars))
