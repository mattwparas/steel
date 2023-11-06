;; Make this macro compile!
(define-syntax do
  (syntax-rules ()
    [(do ((var init step ...) ...) (test expr ...) command ...)
     (letrec* ([loop
                (lambda (var ...)
                  (if test
                      (begin
                        expr ...)
                      (begin
                        command ...
                        (loop (do "step" var step ...) ...))))])
              (loop init ...))]
    [(do "step" x) x]
    [(do "step" x y) y]))

(define (quibble . args)
  (display "{")
  (do ((rem args (cdr rem))
       ;; (foo bar baz) ;; Checking macro expansion
       )
      ((null? rem) (display "}\n"))
      (display (car rem))
      (cond
        [(= 1 (length rem))]
        [(= 2 (length rem)) (display " and ")]
        [else (display ", ")])))

(quibble)
(quibble "ABC")
(quibble "ABC" "DEF")
(quibble "ABC" "DEF" "G" "H")
