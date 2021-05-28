;; Sets of useful ideas for things

(define-syntax match
  (syntax-rules (list)
    [(match list x) x]))

;; Do some pattern matching destructuring with this
;; Pull in the let destruct macro and use that destructuring for lists
;; (displayln (match list 10))

(define-syntax destruct
  (syntax-rules (list)
    [(destruct ((list var) ret-value) body ...)
     (let ((ret ret-value))
       (destruct-list ((var) ret) body ...))]
    [(destruct ((list var1 var2 ...) ret-value) body ...)
     (let ((ret ret-value))
       (destruct-list ((var1 var2 ...) ret) body ...))]))

;; destruct a list
;; maybe write a generic match library?
(define-syntax destruct-list
  (syntax-rules (..)
    [(destruct-list ((var ..) ret-value) body ...)
     (let ((var ret-value))
       (begin body ...))]
    ;; This should throw an error if there are remaining values in the list that
    ;; the pattern matching isn't complete
    [(destruct-list ((var) ret-value) body ...)
     (let ((var (car ret-value)))
       (begin body ...))]
    [(destruct-list ((var1 var2 ...) ret-value) body ...)
     (let ((var1 (car ret-value)))
       (destruct-list ((var2 ...) (cdr ret-value))
                     body ...))]))

;; <var> without the <..> means bind the value there
(destruct ((list a b c) '(1 2 3))
          (displayln a)
          (displayln b)
          (displayln c))

;; <var> .. means bind the rest of the list to that parameter
(destruct ((list a b c ..) '(1 2 3 4 5))
          (displayln a)
          (displayln b)
          (displayln c))
