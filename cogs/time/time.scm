(require-builtin steel/time)
(provide (for-syntax time!))


(define (instant/elapsed->string t)
    (~> t
        (instant/elapsed)
        (duration->string)))

(define-syntax time! 
    (syntax-rules ()
        [(time! expr)
         (let ((t (instant/now))
               (result expr))

            (display (quote expr)) (display " took ")
            (displayln (instant/elapsed->string t))
            result)]))



