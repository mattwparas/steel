(define/contract (applesauce x y z)
    (->/c int? int? int? int?)
    (+ x y z))

(define/contract (dummy foo bar)
    (->/c string? integer? (->/c string? string?))
    (list (int->string bar) foo))

(applesauce 10 20 30)
