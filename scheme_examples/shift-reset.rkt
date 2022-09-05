; #lang racket

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
                                (displayln "### k5 world")
                                ; (displayln k)
                                ; (inspect-bytecode k)
                                ; (displayln v)
                                (reset (k v)))))))))

;; TODO: something is off with the general flow of things here
; (* 2 (reset (+ 1 (shift k (k 5)))))

(* 2 (reset (+ 1 (shift k (begin 
    (displayln "@@@@@@@@@@@@@@@@@")
    (displayln k)
    ; (inspect-bytecode k)
    ; (displayln "@@@@@@@@@@@@@@@@@")
    (k 5)
    
    ; 5

    )))))


; (* 2 (reset (+ 1 (shift k (begin 
;     (displayln "@@@@@@@@@@@@@@@@@")
;     ; (displayln k)
;     (displayln k)
;     (k 5)
;     (displayln "@@@@@@@@@@@@@@@@@")
;     )))))

; (* 2 (+ 1 []))

; (* 2 (+ 1 5))

; (* 2 (reset (+ 1 (shift _ 5))))

; 0    NEWSCLOSURE : 10
; 1    PASS : 0
; 2    PASS : 32888
; 3    NDEFS : 2
; 4    COPYCAPTURECLOSURE : 0
; 5    COPYCAPTURESTACK : 0
; 6    READCAPTURED : 1
; 7    READCAPTURED : 0
; 8    TAILCALL : 1
; 9    POP_PURE : 0
; 10    ECLOSURE : 0
; 11    CALLGLOBALTAIL : 282
; 12    PASS : 1
; 13    POP_PURE : 1