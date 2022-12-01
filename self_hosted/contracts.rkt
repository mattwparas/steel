(define/contract (applesauce x y z)
    (->/c int? int? int? int?)
    (+ x y z))

(define/contract (dummy foo bar)
  (->/c string? integer? (->/c string? string?))
  (list (int->string bar) foo))

;; Function (Listof String)
(define/contract (test lst)
    (->/c (listof string?) (listof string?))
    lst)

;; Function(Any) -> Listof(Any)
(lambda (x) (list x))

;; Function(Any, Number) -> Listof( Any, Number)
(lambda (x y) (list x (+ 1 y)))

(lambda (x y z) (+ x y z)) ;; Function( (int, int, int) -> int )

;; Function ( unknown int ) -> ListOf[ { string, unknown } ]
;; Lists should be default Listof { any }
;; But they should be updated to include more information as it gets collected
;; ListOf can be a contract binding to limit the type information
;; But by default, it can be _anything_, and therefore inference should just
;; continue to be the union of anything that gets added to it - in this case, 
;; list is called concretely with an int and any. Therefore the inferred type should be
;; List of string and any
(lambda (foo bar) (list (int->string bar) foo))

; (applesauce 10 20 "hello world") ;; Type Error => "hello world" is a string, expected int
