(require "other.rkt" ;; Normal import
         (for-syntax "other.rkt") ;; Explicitly for the macros
        ;  "match.rkt"
        ;  (for-syntax "match.rkt")
         )

(define (black-box x) x)

(dummy (black-box #t) (black-box #f))


; (dummy2 (black-box #t)
;     (hello-world))
; (hello-world)

;; -> 
; (test "Matches patterns with constants mixed in"
; (displayln 
;     (match! (list 1 2 3 4 5)
;         ((?x 2 ?y 4 ?z) (+ ?x ?y ?z))))
    ;   (+ 1 3 5))