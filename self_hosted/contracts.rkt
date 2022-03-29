(define/contract (applesauce x y z)
    (->/c int? int? int? int?)
    (+ x y z))

(define/contract (dummy foo bar)
    (->/c string? integer? (->/c string? string?))
    (list (int->string bar) foo))


(lambda (x y z) (+ x y z)) ;; Function( (int, int, int) -> int )

(applesauce 10 20 "hello world") ;; Type Error => "hello world" is a string, expected int
