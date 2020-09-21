(define (some pred lst)
  (cond [(null? lst) #f]
        [(pred (car lst)) #t]
        [else (some pred (cdr lst))]))

(define (can-sum-to target lst)
  (or (equal? target 0)
      (and
       (> target 0)
       (some
        (lambda (m) (can-sum-to (- target m) lst))
        lst))))


;; add some built in memoization to see if we can just avoid situations like this
;; built in memoization would be cool as a language construct


(can-sum-to 7 '(2 3 11))
(can-sum-to 9 '(2 4 6))
;; (can-sum-to 1 '(0 2 3 4))
;; (can-sum-to 15 '(0 1 2 3 4 5))


;; something like this:
;; Adds the function to the cache
;; (define-memoized (blargh)) --> expands to (memoize func-name) ---> uses the function pointer itself
;; Use a memoization LRU cache to keep the cache size limited
;; Specialize the function call because it can't be treated as a normal one...
;; Make a special form for it? Seems a bit much
;; Add a flag to the bytecode struct that says whether or not to memoize it... maybe bad maybe good...


;; Ensure that a function is referentiallly transparent before actually doing any memoization of it
;; This might mean doing some sort of tom foolery with the parsing step in order to catch some things
