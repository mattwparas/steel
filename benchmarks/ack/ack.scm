(provide loop
         ackermann)

(define (ackermann m n)
  (stdout-simple-displayln m " " n)
  (cond
    [(equal? m 0) (+ n 1)]
    [(equal? n 0) (ackermann (- m 1) 1)]
    [else (ackermann (- m 1) (ackermann m (- n 1)))]))

(set! ackermann ackermann)

(define (loop x)
  (if (equal? x 1)
      #true
      (begin
        (ackermann 3 3)
        (loop (+ x 1)))))

(#%jit-compile-2 ackermann)
; (#%jit-compile-2 loop)

(ackermann 3 3)

; (loop 50)

; 0     DynSuperInstruction : 0       ;; m
; 1     PUSHCONST           : 598     ;; " "
; 2     READLOCAL1          : 1       ;; n
; 3     CALLGLOBAL          : 928     ;; stdout-simple-displayln
; 4     FUNC                : 3       ;; stdout-simple-displayln
; 5     POPSINGLE           : 0
; 6     READLOCAL0          : 0       ;; m
; 7     LOADINT0            : 563     ;; 0
; 8     EQUAL2              : 2       ;; equal?
; 9     PASS                : 2       ;; equal?
; 10    IF                  : 16
; 11    READLOCAL1          : 1       ;; n
; 12    LOADINT1            : 589     ;; 1
; 13    ADD                 : 2       ;; +
; 14    PASS                : 2       ;; +
; 15    POPJMP              : 42
; 16    READLOCAL1          : 1       ;; n
; 17    LOADINT0            : 563     ;; 0
; 18    EQUAL2              : 2       ;; equal?
; 19    PASS                : 2       ;; equal?
; 20    IF                  : 29
; 21    READLOCAL0          : 0       ;; m
; 22    LOADINT1            : 589     ;; 1
; 23    SUB                 : 2       ;; -
; 24    PASS                : 2       ;; -
; 25    LOADINT1            : 589     ;; 1
; 26    TCOJMP              : 2       ;; ackermann
; 27    PASS                : 0
; 28    POPJMP              : 42
; 29    READLOCAL0          : 0       ;; m
; 30    LOADINT1            : 589     ;; 1
; 31    SUB                 : 2       ;; -
; 32    PASS                : 2       ;; -
; 33    MOVEREADLOCAL0      : 0       ;; m
; 34    MOVEREADLOCAL1      : 1       ;; n
; 35    LOADINT1            : 589     ;; 1
; 36    SUB                 : 2       ;; -
; 37    PASS                : 2       ;; -
; 38    CALLGLOBAL          : 1230    ;; ackermann
; 39    FUNC                : 2       ;; ackermann
; 40    TCOJMP              : 2       ;; ackermann
; 41    PASS                : 0
; 42    POPPURE             : 2
