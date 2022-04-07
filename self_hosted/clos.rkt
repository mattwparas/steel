;; Lets say we want classes and methods
;; Implementing methods and generics over this should involve creating a runtime class registry
;; For instance: 

(define *registry* (mutable-vector))

;; Class definitions are just dynamic instances of structs, with a variable
;; number of fields

(define (Class name fields methods)
    (mutable-vector 'ClassObject name fields methods))

(define (Class-name self) (mut-vector-ref self 1))
(define (Class-fields self) (mut-vector-ref self 2))
(define (Class-methods self) (mut-vector-ref self 3))


(define Animal (Class 'Animal '(name color weight) (hash)))





;; TODO: mutable structs -> also normal structs really need to be fixed
;; Be able to override the printing of an object via a scheme function -> describe how it gets

;; Class instances get registered with the run time via the class registry
;; Constructors for classes resolve the constructor function via the registry
;; Inheritance chain is solved dynamically via method resolution
;; Class instance should store information about its parents

; (define (make-class class)
;     (vector-push! class))


