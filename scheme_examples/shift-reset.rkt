(define-syntax reset 
    (syntax-rules ()
        ((reset ?e) (*reset (lambda () ?e)))))

(define-syntax shift 
    (syntax-rules ()
        ((shift ?k ?e) (*shift (lambda (?k) ?e)))))

(define (*meta-continuation* v)
    (error "You forgot the top-level reset..."))

(define (*abort thunk) 
    (let ((v (thunk)))
        (*meta-continuation* v)))

(define (*reset thunk)
    (let ((mc *meta-continuation*))
        (call/cc (lambda (k)
            (begin
                (set! *meta-continuation*
                        (lambda (v)
                            (set! *meta-continuation* mc) 
                            (k v)))
                (*abort thunk))))))

(define (*shift f)
    (call/cc
        (lambda (k)
            (*abort (lambda ()
                        (f (lambda (v)
                                (reset (k v)))))))))

(* 2 (reset (+ 1 (shift k (k 5)))))

;; In theory, this seems to work for our shift and reset business
;; Now, its not performant, but you seem to only pay the price only if you use the code
(let () 
    (reset
        (call-with-exception-handler (lambda (x) (displayln x) (shift k (k void))) 
            (lambda () (+ 10 20 (error "oops!")))))
    (displayln "hi"))