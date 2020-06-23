(export def-method impl)

(define-syntax def-method
  (syntax-rules ()
    [(def-method struct-name (define (a this b ...) body ...))
     (define ((datum->syntax struct-name . a) this b ...)
       (unless ((datum->syntax struct-name ?) this)
         (error! (datum->syntax struct-name . a) "method takes a value of" struct-name "given" this))
       body ...)]))


(define-syntax impl
  (syntax-rules ()
    [(impl struct-name (define (a this b ...) body ...) c ...)
     (begin (def-method struct-name (define (a this b ...) body ...))
            (impl struct-name c ...))]
    [(impl struct-name (define (a this b ...) body ...))
     (def-method struct-name (define (a this b ...) body ...))]))


;; (struct Apple (a b c))
;; (impl Apple
;;       (define (eat this number)
;;         (+ number 25))
;;       (define (color this number)
;;         (+ number 50)))
