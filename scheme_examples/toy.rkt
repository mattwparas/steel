;; Sets of useful ideas for things
;; #lang racket

;(define-syntax match
;  (syntax-rules (list)
;    [(match list x) x]))

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


(define-syntax match-exact
  (syntax-rules ()
    [(match-exact 1)
     10]
    [(match-exact 2)
     20]
    [(match-exact 'test)
     30]))


(displayln (match-exact 'test))

;; evaluates body if expr is a list
;; errors otherwise
(define-syntax match-list
  (syntax-rules ()
    [(match-list expr body)     
     (let ((exp expr))
       (if (list? exp)
           body
           (error! "match expected a list, found: " exp)))]))

; (match-list (list 1 2 3 4 5) (displayln "Found a list!"))


(define (matcher pattern expr)
  (cond
    [(list? pattern) 
      (displayln "found a list pattern")]
    [(symbol? pattern) (displayln "found a symbol pattern")]
    [else (displayln "Could not match")]))


(matcher '(list 1 2 3) (list 1 2 3))


;(define-syntax cond
;  (syntax-rules (else)
;    [(cond [else e1 ...])
;     (begin e1 ...)]
;    [(cond [e1 e2 ...])
;     (when e1 e2 ...)]
;    [(cond [e1 e2 ...] c1 ...)
;     (if e1
;         (begin e2 ...)
;         (cond c1 ...))]))

;(define-syntax match-and-bind
;  (syntax-rules ()
;    [(match-and-bind expr pat)
;     (


;; What is a pattern?
;; It is either a '?
;; Or it is either a 

;; (define (match-list pattern lst)

      

;(define-syntax match-and-bind
;  (syntax-rules ()
;    ;; else case - unable to find a match, just run the expression given
;    [(match-and-bind expr
;                     [else e1 ...])
;     (begin e1 ...)]
;    [(match-and-bind expr
;                     [(pat body) rest ...])]))
     
                    
           

;(define-syntax dispatch
;  (syntax-rules (..)
;    [(dispatch (


















