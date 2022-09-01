(define-syntax reset (syntax-rules ()
    ((reset ?e) (*reset (lambda () ?e)))))

(define-syntax shift (syntax-rules ()
    ((shift ?k ?e) (*shift (lambda (?k) ?e)))))

(define (*meta-continuation* v)
    (error! "You forgot the top-level reset..."))

(define (*abort thunk) 
    ; (let ((v (thunk)))
        (*meta-continuation* (thunk)))

(define (*reset thunk)
    (let ((mc *meta-continuation*))
        (call/cc (lambda (k)
            (begin
                (set! *meta-continuation*
                (lambda (v)
                    (set! *meta-continuation* mc) (k v)))
                    (*abort thunk))))))

(define (*shift f)
    (call/cc
        (lambda (k)
            (*abort (lambda ()
                        (f (lambda (v)
                                (reset (k v)))))))))

(* 2 (reset (+ 1 (shift k (k 5)))))