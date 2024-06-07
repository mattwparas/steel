;; destruct works like so:
;; (destruct (a b c) value)
;;  ...
;; (define a (car value)
;; (define b (car (cdr value)))
;; (define c (car (cdr (cdr value))))
(define-syntax destruct
  (syntax-rules ()
    [(destruct (var) ret-value)
     (define (datum->syntax var)
       (car ret-value))]
    [(destruct (var1 var2 ...) ret-value)
     (begin
       (define (datum->syntax var1)
         (car ret-value))
       (destruct (var2 ...) (cdr ret-value)))]))

(define-syntax def-method
  (syntax-rules ()
    [(def-method struct-name (define/method (a this b ...) body ...))
     (define ((datum->syntax struct-name |.| a) this b ...)
       (unless ((datum->syntax struct-name ?) this)
         (error! (datum->syntax struct-name |.| a) "method takes a value of" struct-name "given" this))
       body ...)]))

;; impl block asserts that each function contains the struct type given as the first argument
;; This is why later down we use the thread first vs. the thread last given above
;;
;; However, take note that a recursive call will not work properly in this, best to be used as an interface
;; since it does not transform the name of the recursive call
(define-syntax impl
  (syntax-rules ()
    [(impl struct-name (define/method (a this b ...) body ...) c cs ...)
     (begin
       (def-method struct-name (define/method (a this b ...) body ...))
       (impl struct-name c cs ...))]
    [(impl struct-name (define/method (a this b ...) body ...))
     (def-method struct-name (define/method (a this b ...) body ...))]))

(struct Stack (lst) #:transparent)
(impl
 Stack
 ;; Change this to be something like (define/method)
 ;; as to disambiguate it from the base define
 (define/method (pop stack)
                (define contents (Stack-lst stack))
                (if (null? contents) '(#f '()) (list (car contents) (cdr contents))))
 (define/method (push stack value) (define contents (Stack-lst stack)) (Stack (cons value contents))))

(-> (Stack '()) (Stack.push 1) (Stack.push 2) (Stack.push 3) (Stack.push 4) (Stack.pop))

; (Stack.pop
;     (%plain-let
;         ((x (%plain-let
;                 ((x (%plain-let
;                         ((x (%plain-let ((x (Stack (quote ()))))
;                                 (Stack.push x 1))))
;                             (Stack.push x 2))))
;                             (Stack.push x 3))))
;                             (Stack.push x 4)))

; (define test-stack (Stack '()))

; (destruct (pop-val-test new-stack-test)
;         (-> test-stack
;             (Stack.push 1)
;             (Stack.push 2)
;             (Stack.push 3)
;             (Stack.push 4)
;             (Stack.pop)))

; pop-val-test ;; => 4
; new-stack-test ;; => '(3 2 1)
; (assert! (equal? 4 pop-val-test))
; (assert! (equal? '(3 2 1) new-stack-test))
