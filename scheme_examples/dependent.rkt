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

; (foo 1 2)

(define (all func lst)
  (if (null? lst)
      #t
      (if (func (car lst))
          (all func (cdr lst))
          #f)))

(define (all/c func lst)
    (make/c (all func lst) (list 'all func lst)))

(define string?/c (make/c string? 'string?/c))
(define symbol?/c (make/c symbol? 'symbol?/c))
(define integer?/c (make/c integer? 'integer?/c))
(define float?/c (make/c float? 'float/c))
; (define )

(define (lst-func-loop funcs args)
    (if (empty? funcs) 
        #t
        (let ((result ((car funcs) (car args))))
            (if result
                (lst-func-loop (cdr funcs) (cdr args))
                #f))))

;; Get the contract that uniquely identifies this predicate
;; Down to the predicate - we want to match the predicates along the way
;; Doesn't need to explicitly match this way - could do it other ways but this way works nicely
; (define (make-identity-pred obj)
;     (cond [(string? obj) string?/c]
;           [(symbol? obj) symbol?/c]
;           [(integer? obj) integer?/c]
;           [(float? obj) float?/c]
;           ;; If its a struct, get the contracts for all of the children fields as well
;           ;; creating an exact copy of the shape of the struct on the way back up
;           [(struct? obj)
;             (make/c 
;                 (let ((struct-list (struct->list obj)))
;                 (lambda (input)
;                     ; (let ((struct-list (struct->list obj)))
;                         (displayln obj)
;                         ; (displayln struct-list)
;                         ; (displayln input)
;                         (if (struct? input)
;                             (let ((input-list-struct (struct->list input)))
;                                 ;; If the structs match the name/type, check the children
;                                 (and (equal? (car struct-list) (car input-list-struct))
;                                     (let ((children (map make-identity-pred (cdr struct-list))))
;                                             ; (displayln children)
;                                             ; (displayln input-list-struct)
;                                             (lst-func-loop
;                                                 (map make-identity-pred (cdr struct-list))
;                                                 (cdr input-list-struct)))))
                                            
;                         #f))) 'struct-contract)]))


(define (make-identity-pred obj)
    (cond [(string? obj) string?/c]
          [(symbol? obj) symbol?/c]
          [(integer? obj) integer?/c]
          [(float? obj) float?/c]
          ;; If its a struct, get the contracts for all of the children fields as well
          ;; creating an exact copy of the shape of the struct on the way back up
          [(struct? obj)
            (make/c 
                ;; TODO uncomment this and comment below, works
                (test-let ((struct-list (struct->list obj)))
                (lambda (input)
                    ; (test-let ((struct-list (struct->list obj)))
                        (if (struct? input)
                            (test-let ((input-list-struct (struct->list input)))
                                ;; If the structs match the name/type, check the children
                                (and (equal? (car struct-list) (car input-list-struct))
                                    (test-let ((children (map make-identity-pred (cdr struct-list))))
                                            ; (displayln children)
                                            ; (displayln input-list-struct)
                                            (lst-func-loop
                                                (map make-identity-pred (cdr struct-list))
                                                (cdr input-list-struct)))))
                                            
                        #f))) 'struct-contract)]))

(struct Applesauce (a b c))
(struct Bananas (foo bar baz))

(define identity-contract (make-identity-pred (Applesauce "test" 'hello (Bananas 1 2 3))))

; (define identity-contract (make-identity-pred (Applesauce (Bananas 1 2 3) 'hello 10)))

(displayln (identity-contract (Applesauce "blagh" 'test (Bananas 3 4 5))))



;; Identity contract
;; Get the predicate that satisfies the most limited type information for a given func
;; Returns the contract that satisfies a given type most succinctly?

(struct Some (value))
(struct None ())

;; Contracts for option
(define (Option/c pred)
    (make/c (fn (x) 
                (cond [(Some? x) (pred (Some-value x))]
                      [(None? x) #t]
                      [else #f])) 
            'Option/c))

(define (Option? value)
    (or (Some? value) (None? value)))

(define (inner-value-contract option)
    (cond [(Some? option) (make-identity-pred (unwrap-some option))]
          [(None? option) any/c]
          [else any/c]))

;; Map - explore dynamic dispatch with contracts?
(define/contract (map-option option func)
    (->i ([option Option?]
          [func (option) (->/c (inner-value-contract option) any/c)])
          [result Option?])

    (cond [(Some? option) (Some (func (Some-value option)))]
          [(None? option) (None)]))

;; Get the inner value of the option - contract checking along the way
;; Figure out how to turn off contracts on OptLevel3 regardless - 
;; this would speed up performance a lot - also figure out how to map this to compile time options in Rust
(define (unwrap-some option)
    (Some-value option))


(define test (Some 10))

; (define/contract (add10 word)
;     (->/c string? string?)
;     word)

; (displayln (map-option test (lambda (x) (string->int x))))



; (contract/out o-map (->/c (t : predicate?) (Option/c t) (->/c t Option?) Option?))

; (define (map-option option func)
;     (cond [(Some? option) (Some (func (Some-value option)))]
;           [(None? option) option]))

;; Define a generic that is then bound and enforced to be equivalent for each of the fields
;; For instance:
; (->t [(K : predicate?)
;       (V : predicate?)]

;       (Option/c t)

; (->i ([option Option?]
;       [func (option) (and/c (->/c _ Option?) )]

; ))

;; Implement generics as well
;; dependent contracts could take an optional parameter that must be unified - i.e. the same

; (->i ([x number?]
;         [y (x) (>=/c x)])
;         [lolol (x y) (and/c number? (>=/c (+ x y)))])