(require "dll.scm")
(require "steel/tests/unit-test.scm"
         (for-syntax "steel/tests/unit-test.scm"))

(define __dll-module 'dll-module)

(provide __dll-module)

(define (run)
  (let ([dlist (dllist #f #f)])
    (insert-head dlist 1)
    (displayln dlist)
    (insert-tail dlist 4)
    (displayln dlist)
    (insert-after dlist (dllist-head dlist) 2)
    (displayln dlist)
    (let* ([next-to-last (insert-before dlist (dllist-tail dlist) 3)]
           [bad-link (insert-before dlist next-to-last 42)])
      (remove-link dlist bad-link))
    (displayln dlist)
    (displayln (dllist-elements dlist))
    (displayln dlist)))

(test-module "dll-tests"
             (check-equal? "creating dll with cycle works"
                           #true
                           (begin
                             (run)
                             #true)))

;; Testing deep collections
(struct ConsCell (car cdr) #:mutable #:transparent)

(define (build-list x)
  (cond
    [(equal? x 100000) void]
    [else (ConsCell x (build-list (+ x 1)))]))

(define (build-nested-list x y)
  (cond
    [(equal? x y) void]
    [else (ConsCell x (build-nested-list (+ x 1) y))]))

(define (build-hashmap-chain x)
  (if (equal? x 100000) (hash 'a x) (hash 'a (build-hashmap-chain (+ x 1)))))

(define (build-list-chain x)
  (if (equal? x 10000) (list x) (list x (build-list-chain (+ x 1)))))

; (define (test-depth)

;   (define (build-deep-hash x)
;     (if (equal? x 100000) (hash x x) (hash (build-deep-hash (+ x 1)) x)))

;   (define m (build-deep-hash 0))
;   (define o (build-deep-hash 0))

;   (equal? m o))

(test-module "deep recursive data structures"
             ; (check-equal? "deep hash structures" #true (test-depth))
             (check-equal? "dropping deep list doesn't panic"
                           #true
                           (let ([foo (build-nested-list 0 100000)]) #true))
             (check-equal? "dropping deep built in list doesn't panic"
                           #true
                           (let ([foo (build-list-chain 0)])

                             #true))
             (check-equal? "dropping deep hashmap chain does't panic"
                           #true
                           (let ([foo (build-hashmap-chain 0)]) #true)))
