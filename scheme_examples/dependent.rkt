; #lang racket


;; the symbols that each of the conditions accepts must be validated
;; there should be a map of symbol to argument as well
;; i.e. x -> 1st, y -> 2nd
;; Then (x y) -> 1st and 2nd
;; This map needs to be validated by the input function in some capacity
;;
;; this also needs to be a macro to create a function with the operand
;;   i.e. (>=/c x) -> (lambda (x) (>=/c x)) ;; -> in the run time, this needs to be 
;;                                                applied twice, once for the dependencies, once for the argument
;;
;; Each argument should only be evaluated once, and the result should be captured

; (foo 1 2)


; (x) (>=/c x) -> (lambda (x) (>=/c x))
(define-syntax contract-wrap 
    (syntax-rules ()
        [(contract-wrap args pat) 
         (lambda (args) pat)]))

;; Takes a precondition, and returns a list to be consumed by a built-in
;; returns a list of the identifier for the argument, the list of arguments to the input function, 
;; and the thunk that returns the function for the contract
(define-syntax dependent-condition
    (syntax-rules ()
        [(dependent-condition (ident func))
         (list 'ident '() (lambda () (make/c func 'func )) 'func )]
        [(dependent-condition (ident (arg) func))
         (list 'ident '(arg) (lambda (arg) (make/c func 'func )) 'func )]
        [(dependent-condition (ident (args ...) func))
         (list 'ident '(args ...) (lambda (args ...) (make/c func 'func )) 'func )]))


(define-syntax ->i
  (syntax-rules ()
    [(->i r) ;; alias ->i to ->/c
     (->/c r)]
    [(->i (a) b)
     (make-dependent-function/c (dependent-condition a) (dependent-condition b))]
    [(->i (a b) c)
     (make-dependent-function/c (dependent-condition a) (dependent-condition b) (dependent-condition c))]

    ; [(->/i a b c d)
    ;  (make-function/c (make/c a 'a) (make/c b 'b)
    ;                   (make/c c 'c) (make/c d 'd))]
    ; [(->/i a b c d e)
    ;  (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
    ;                   (make/c d 'd) (make/c e 'e))]
    ; [(->/i a b c d e f)
    ;  (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
    ;                   (make/c d 'd) (make/c e 'e) (make/c f 'f))]
    ; [(->/i a b c d e f g)
    ;  (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
    ;                   (make/c d 'd) (make/c e 'e) (make/c f 'f)
    ;                   (make/c g 'g))]
    ; [(->/i a b c d e f g h)
    ;  (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
    ;                   (make/c d 'd) (make/c e 'e) (make/c f 'f)
    ;                   (make/c g 'g) (make/c h 'h))]
    ; [(->/i a b c d e f g h i)
    ;  (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
    ;                   (make/c d 'd) (make/c e 'e) (make/c f 'f)
    ;                   (make/c g 'g) (make/c h 'h) (make/c i 'i))]
                      
                      ))

; (define/contract (foo x y)
;     (->/c (>=/c 10) (>=/c 20) (>=/c 30))
;     (+ x y))

; (displayln (foo 5 20))

;; - make-dependent/c
;; - each precondition should have (list of args, contract, name)
;; - the postcondition should have 



; (define (make-dependent/c ))

(define/contract (foo x y)
    (->i ([x number?]
          [y (x) (>=/c x)])
          [lolol (x y) (and/c number? (>=/c (+ x y)))])
    (+ x y 1))

(foo 1 2)


(contract/out o-map (->/c (t : predicate?) (Option/c t) (->/c t Option?) Option?))

(define (map-option option func)
    (cond [(Some? option) (Some (func (Some-value option)))]
          [(None? option) option]))

;; Define a generic that is then bound and enforced to be equivalent for each of the fields
;; For instance:
(->t [(K : predicate?)
      (V : predicate?)]

      (Option/c t)

; (->i ([option Option?]
;       [func (option) (and/c (->/c _ Option?) )]

; ))

;; Implement generics as well
;; dependent contracts could take an optional parameter that must be unified - i.e. the same

; (->i ([x number?]
;         [y (x) (>=/c x)])
;         [lolol (x y) (and/c number? (>=/c (+ x y)))])