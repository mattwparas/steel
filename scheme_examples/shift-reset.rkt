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
; (let () 
;     (reset
;         (call-with-exception-handler (lambda (x) (displayln x) (shift k (k void))) 
;             (lambda () (+ 10 20 (error "oops!")))))
;     (displayln "hi"))


(define-syntax with-handler
    (syntax-rules ()
        [(with-handler handler expr)
         (reset (call-with-exception-handler (lambda (err) (handler err) (shift k (k void)))
                    (lambda () expr)))]))

(with-handler
    (lambda (err) 
        (displayln err) 
        (displayln "Going back to the main thread of execution..."))
    (+ 10 20 30 "blagh"))

; (displayln "I made it out")

; ; (let ()
; (with-handler
;     (lambda (err) 
;         (displayln err) 
;         (error "Going back to the main thread of execution..."))
;     (let ()
;         (with-handler (lambda (err) (displayln "inner error!") (error "applesauce"))
;             (+ 10 20 (error "uh oh")))))

; (displayln "outside the loop")

