

(define program '(



(define-syntax ->/c
  (syntax-rules ()
    [(->/c r)
     (make-function/c (make/c r 'r))]
    [(->/c a b)
     (make-function/c (make/c a 'a) (make/c b 'b))]
    [(->/c a b c)
     (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c))]
    [(->/c a b c d)
     (make-function/c (make/c a 'a) (make/c b 'b)
                      (make/c c 'c) (make/c d 'd))]
    [(->/c a b c d e)
     (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
                      (make/c d 'd) (make/c e 'e))]
    [(->/c a b c d e f)
     (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
                      (make/c d 'd) (make/c e 'e) (make/c f 'f))]
    [(->/c a b c d e f g)
     (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
                      (make/c d 'd) (make/c e 'e) (make/c f 'f)
                      (make/c g 'g))]
    [(->/c a b c d e f g h)
     (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
                      (make/c d 'd) (make/c e 'e) (make/c f 'f)
                      (make/c g 'g) (make/c h 'h))]
    [(->/c a b c d e f g h i)
     (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
                      (make/c d 'd) (make/c e 'e) (make/c f 'f)
                      (make/c g 'g) (make/c h 'h) (make/c i 'i))]))

;; Macro for basic usage of contracts
(define-syntax define/contract
  (syntax-rules ()
    [(define/contract (name args ...)
       contract
       body ...)
     (define name (bind/c contract (lambda (args ...) body ...) 'name))]
    [(define/contract name contract expr)
     (define name ((bind/c
                      (make-function/c (make/c contract 'contract))
                      (lambda () expr))))]))

    (define/contract (applesauce x y z)
        (->/c int? int? int? int?)
        (+ x y z))

    ; (define/contract (bananas x y)
    ;     (->/c string? int? int?)
    ;     (displayln x)
    ;     (+ y 20))
))

(expand! program)

; (define *engine* (Engine::new))

;; Evaluate the program, and interact with the runtime
; (run! *engine* program)

; (get-value *engine* "applesauce")
; (get-value *engine* "bananas")