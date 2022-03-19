

(define program '(
    (define/contract (applesauce x y z)
        (->/c int? int? int? int?)
        (+ x y z))

    (define/contract (bananas x y)
        (->/c string? int? int?)
        (displayln x)
        (+ y 20))
))

(define *engine* (Engine::new))

;; Evaluate the program, and interact with the runtime
(run! *engine* program)

(get-value *engine* "applesauce")
(get-value *engine* "bananas")