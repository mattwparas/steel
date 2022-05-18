;; Add an option to variable bindings that make it so they're frozen
;; Attempting to set! a variable that has been tagged as immutable results in a compile time
;; error
(define test #:immutable 10)
(define *appelsauce #:immutable 20)

;; monday, tuesday, wednesday, thursday, friday