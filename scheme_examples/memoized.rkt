;; function memoizedFibonacci() {
;;   let cache = {};

;;   function fibonacci(n) {
;;     if(n in cache) return cache[n];
;;     if(n <= 1) {
;;       cache[n] = n;
;;     } else {
;;       cache[n] = fibonacci(n - 2) + fibonacci(n - 1);
;;     }
;;     return cache[n];
;;   };
;;   return fibonacci;
;;   }


;; (define (memoFib n)
;;   (define (fibCache n cache)
;;     (cond [(hash-contains? cache n) (hash-get cache n)]
;;           [(<= n 1)]


;;           )
;;     (if (contains cache n)

;;         )

;; Do some analysis on how to annotate functions w/ effects
;; I think it would be a cool experiment to try that
;; Add a type system...? haha jk... unless?

;; special form that automatically memoizes your arguments to the function
;; uses some magic in the VM to measure the context of the program
;; Just capture the pointer to the function call and memoize entirely
;; the onus is on the user to make sure there are no side effects done
;; and that it is purely computation and not effect-ful
(define/memoized (fib n) ...)

(1 . + . 2)





;; (allow mutable-references)
;; (#mut)


;; (define (memoizedFib n)
;;   (define cache (hash))
;;   (define (fibonacci n)
;;     (if (<= n 1) )

;;     )
