
;; Creates a tagged vector -> just make the methods that operate on this vector
;; Somehow these need to be generated into source code, because otherwise it becomes goofy
;; These should just be macros for the generated top level definitions...
;; (struct Applesauce (a b c))

;; (struct Applesauce (a b c) #:mutable) ;; => generates a mutable struct, which uses a different representation under the hood

;; Use this marker to do pointer equality, and redefine the vector? function to exclude structs that
;; have this marker
(define _marker_ (vector 'StructObject))


;; Walk down and decide what to use as the underlying data structure
;; Include this in the standard library - use a marker with pointer equality.
(define-syntax make-struct
  (syntax-rules ()
    [(make-struct struct-name (field))
     (begin (define struct-name 'constructor)
            (define (datum->syntax struct-name - field) 'last-getter))]

    [(make-struct struct-name (field fields ...))
     (begin
       (define (datum->syntax struct-name - field) 10)
       (define (datum->syntax struct-name - ?))
       (make-struct struct-name (fields ...)))]))



;; Make the struct, add getters
(make-struct Sherman (name color weight))
