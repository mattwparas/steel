(provide match-let*)

(define-syntax match-let*
  (syntax-rules ()
    [(match-let* ()
       body ...)

     (begin
       body ...)]

    [(match-let* ([pat expr] rest ...)
       body ...)

     (match expr
       [pat
        (match-let* (rest ...)
          body ...)])]))

;; Example:
; (match-let* ([(list a b) '(1 2)] [(list x ...) (list 1 2 3 4)])
;   (list b a x))
