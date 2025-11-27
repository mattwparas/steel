(provide loop
         ackermann)

(define (ackermann m n)
  ; (stdout-simple-displayln m " " n)
  (cond
    [(equal? m 0) (+ n 1)]
    [(equal? n 0) (ackermann (- m 1) 1)]
    [else (ackermann (- m 1) (ackermann m (- n 1)))]))

(set! ackermann ackermann)

(define (loop x)
  (if (equal? x 1)
      #true
      (begin
        ; (ackermann 3 3)
        (stdout-simple-displayln "hi")
        (loop (+ x 1)))))

; (#%jit-compile-2 ackermann)
(#%jit-compile-2 loop)

; (ackermann 1 1)

; (loop 50)

; 0     DynSuperInstruction : 0       ;; m
; 1     LOADINT0            : 563     ;; 0
; 2     EQUAL2              : 2       ;; equal?
; 3     PASS                : 2       ;; equal?
; 4     IF                  : 10
; 5     READLOCAL1          : 1       ;; n
; 6     LOADINT1            : 589     ;; 1
; 7     ADD                 : 2       ;; +
; 8     PASS                : 2       ;; +
; 9     POPJMP              : 36
; 10    READLOCAL1          : 1       ;; n
; 11    LOADINT0            : 563     ;; 0
; 12    EQUAL2              : 2       ;; equal?
; 13    PASS                : 2       ;; equal?
; 14    IF                  : 23
; 15    READLOCAL0          : 0       ;; m
; 16    LOADINT1            : 589     ;; 1
; 17    SUB                 : 2       ;; -
; 18    PASS                : 2       ;; -
; 19    LOADINT1            : 589     ;; 1
; 20    TCOJMP              : 2       ;; ackermann
; 21    PASS                : 0
; 22    POPJMP              : 36
; 23    READLOCAL0          : 0       ;; m
; 24    LOADINT1            : 589     ;; 1
; 25    SUB                 : 2       ;; -
; 26    PASS                : 2       ;; -
; 27    MOVEREADLOCAL0      : 0       ;; m
; 28    MOVEREADLOCAL1      : 1       ;; n
; 29    LOADINT1            : 589     ;; 1
; 30    SUB                 : 2       ;; -
; 31    PASS                : 2       ;; -
; 32    CALLGLOBAL          : 1220    ;; ackermann
; 33    FUNC                : 2       ;; ackermann
; 34    TCOJMP              : 2       ;; ackermann
; 35    PASS                : 0
; 36    POPPURE             : 2
