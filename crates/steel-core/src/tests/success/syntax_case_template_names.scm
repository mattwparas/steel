;; Variables a syntax-case template binds don't capture the use site's
(define-syntax (swap! stx)
  (syntax-case stx ()
    [(_ a b)
     #'(let ([tmp a])
         (set! a b)
         (set! b tmp))]))

(define (swap-tmp)
  (let ([tmp 1]
        [other 2])
    (swap! tmp other)
    (list tmp other)))
(assert! (equal? (swap-tmp) '(2 1)))

(define-syntax (my-or stx)
  (syntax-case stx ()
    [(_ a b)
     #'(let ([t a])
         (if t t b))]))

(define (or-with-t t)
  (my-or #f t))
(assert! (equal? (or-with-t 7) 7))

;; A syntax-case macro that expands into a syntax-rules one
(define-syntax swap-rules!
  (syntax-rules ()
    [(_ a b)
     (let ([tmp a])
       (set! a b)
       (set! b tmp))]))

(define-syntax (swap-through stx)
  (syntax-case stx ()
    [(_ a b) #'(swap-rules! a b)]))

(define (swap-through-tmp)
  (let ([tmp 1]
        [other 2])
    (swap-through tmp other)
    (list tmp other)))
(assert! (equal? (swap-through-tmp) '(2 1)))

;; Names a caller passes in mean what they mean at the use site
(define-syntax (apply-to stx)
  (syntax-case stx ()
    [(_ f v) #'(f v)]))

(define (passed-in v)
  (let ([car (lambda (x) 'local)])
    (apply-to car v)))
(assert! (equal? (passed-in '(1 2)) 'local))

;; define-values is written with syntax-case
(define (values-under-let)
  (let ([list 'mine])
    (define-values (a b) (values 1 2))
    (+ a b)))
(assert! (equal? (values-under-let) 3))

;; Free names mean the global or builtin, whatever the use site binds
(define (later x)
  (list 'global x))

(define-syntax (call-later stx)
  (syntax-case stx ()
    [(_ v) #'(later v)]))

(define (later-under-let v)
  (let ([later (lambda (x) 'local)])
    (list (later v) (call-later v))))
(assert! (equal? (later-under-let 1) '(local (global 1))))

(define (later-under-param later)
  (call-later later))
(assert! (equal? (later-under-param 5) '(global 5)))

(define-syntax (first-of stx)
  (syntax-case stx ()
    [(_ l) #'(car l)]))

(define (first-under-let lst)
  (let ([car cdr])
    (list (car lst) (first-of lst))))
(assert! (equal? (first-under-let '(1 2)) '((2) 1)))

(define (first-under-define lst)
  (define car cdr)
  (first-of lst))
(assert! (equal? (first-under-define '(1 2)) 1))

;; Templates put together with quasisyntax
(define-syntax (add-all stx)
  (syntax-case stx ()
    [(_ x ...)
     #`(let ([total 0])
         #,@(map (lambda (v) #`(set! total (+ total #,v))) (syntax-e #'(x ...)))
         total)]))

(define (add-all-with-total total)
  (add-all total 1 2))
(assert! (equal? (add-all-with-total 10) 13))

;; The use site's names aren't pattern variables, even when they share a name with one
(define (add-all-with-x x)
  (add-all x 1 2))
(assert! (equal? (add-all-with-x 10) 13))

(define-syntax (later-of-each stx)
  (syntax-case stx ()
    [(_ v ...) #`(list #,@(map (lambda (e) #`(later #,e)) (syntax-e #'(v ...))))]))

(define (later-of-each-under-let a)
  (let ([later 'local])
    (later-of-each a 2)))
(assert! (equal? (later-of-each-under-let 1) '((global 1) (global 2))))

;; A binder made from the input refers to the use site's name
(define-syntax (with-escape stx)
  (syntax-case stx ()
    [(k escape body ...)
     #'(call-with-current-continuation (lambda (escape)
                                         body ...))]))

(assert! (equal? (with-escape leave (+ 1 (leave 5))) 5))

;; Definitions the template names stay with that expansion, ones the use site names don't
(define-syntax (define-with-helper stx)
  (syntax-case stx ()
    [(_ name v)
     #'(begin
         (define helper v)
         (define (name)
           helper))]))

(define helper 'outer)
(define-with-helper get-one 1)
(define-with-helper get-two 2)
(assert! (equal? (list (get-one) (get-two) helper) '(1 2 outer)))

;; Variables the template binds itself, with the forms that bind them
(define-syntax (count-to stx)
  (syntax-case stx ()
    [(_ n)
     #'(let loop ([i 0])
         (if (< i n)
             (loop (+ i 1))
             i))]))
(assert! (equal? (count-to 3) 3))

(define (count-under-let i loop)
  (list i loop (count-to 2)))
(assert! (equal? (count-under-let 'a 'b) '(a b 2)))

(define-syntax (bind-all stx)
  (syntax-case stx ()
    [(_ v)
     #'(let ([a v])
         (define b (+ a 1))
         ((lambda (c) (list a b c)) (+ b 1)))]))
(assert! (equal? (bind-all 1) '(1 2 3)))

(define (bind-all-under-let a b c)
  (list a b c (bind-all 10)))
(assert! (equal? (bind-all-under-let 1 2 3) '(1 2 3 (10 11 12))))

;; A name a template binds in one place is still free in another
(define (helper-of x) (list 'global x))

(define-syntax (both stx)
  (syntax-case stx ()
    [(_ e) #'(cons (let ([helper-of 5]) helper-of) (helper-of e))]))

(define (both-under-let)
  (let ([helper-of (lambda (x) 'local)])
    (both 1)))
(assert! (equal? (both-under-let) '(5 global 1)))

;; A builtin a template names isn't the use site's variable
(define-syntax (wrap-in-list stx)
  (syntax-case stx ()
    [(_ e) #'(list e)]))

(define (wrap-under-let)
  (let ([list (lambda (x) 'local)])
    (wrap-in-list 1)))
(assert! (equal? (wrap-under-let) '(1)))

;; Symbols from syntax are the ones written in the program
(define-syntax (symbol-of stx)
  (syntax-case stx ()
    [(_ a) (let ([v (syntax-e #'a)])
             (if (eq? v 'wanted) #''same #''different))]))
(assert! (equal? (list (symbol-of wanted) (symbol-of other)) '(same different)))
