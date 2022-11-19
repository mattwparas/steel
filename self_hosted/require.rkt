; #lang racket

; (require "provide.rkt")

;; TODO: if _all_ modules just get loaded and prefixed with their name, we should be okay
;; and subsequently, if modules in loaded modules get prefixed with 
(require "unit-test.rkt" 
         (for-syntax "unit-test.rkt"))

; (displayln list)

; (displayln ((get-apple)))
; (mutate-apple)
; (displayln ((get-apple)))

; (Applesauce 1 2 3)

(test-module
    (check-equal? "Applesauce bananas" 
                  (+ 10 20 30) 
                  (+ 10 20 30)))
