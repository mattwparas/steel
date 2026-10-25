;; Macros whose templates define macros. Each use of the generated macro
;; gets its own copies of the names its template binds.

(define-syntax gen-define
  (syntax-rules ()
    [(_ name) (define-syntax name (syntax-rules () [(_ get v) (define get v)]))]))
(gen-define mk-define)
(mk-define get1 1)
(assert! (= get1 1))

(define-syntax gen-pair
  (syntax-rules ()
    [(_ name)
     (define-syntax name
       (syntax-rules ()
         [(_ get v) (begin (define t v) (define (get) t))]))]))
(gen-pair mk)
(mk first-getter 1)
(mk second-getter 2)
(assert! (equal? (list (first-getter) (second-getter)) '(1 2)))

(define (in-a-body)
  (mk g1 10)
  (mk g2 20)
  (list (g1) (g2)))
(assert! (equal? (in-a-body) '(10 20)))

(define (generated-in-a-body)
  (define-syntax gen
    (syntax-rules ()
      [(_ name)
       (define-syntax name
         (syntax-rules ()
           [(_ get v) (begin (define t v) (define (get) t))]))]))
  (gen local-mk)
  (local-mk g1 10)
  (local-mk g2 20)
  (list (g1) (g2)))
(assert! (equal? (generated-in-a-body) '(10 20)))

;; A name the outer template binds is still seen by the inner one
(define-syntax gen-reader
  (syntax-rules ()
    [(_ name)
     (define-syntax name
       (syntax-rules ()
         [(_ get)
          (let ([acc 5])
            (define-syntax get (syntax-rules () [(_) acc]))
            (get))]))]))
(gen-reader reader)
(assert! (= (reader g) 5))

;; The generated macro's template names the locals where the outer one was
;; defined, whatever is bound where it's used
(define (uses-a-local y)
  (define-syntax gen (syntax-rules () [(_ name) (define-syntax name (syntax-rules () [(_) y]))]))
  (gen get)
  (get))
(assert! (= (uses-a-local 4) 4))

(define (uses-a-shadowed-local x)
  (define-syntax gen (syntax-rules () [(_ name) (define-syntax name (syntax-rules () [(_ v) (+ x v)]))]))
  (let ([x 50])
    (gen add)
    (let ([x 99]) (add 1))))
(assert! (= (uses-a-shadowed-local 1) 2))

(define (with-a-define y)
  (define-syntax gen
    (syntax-rules ()
      [(_ name) (begin (define z (* y 2)) (define-syntax name (syntax-rules () [(_) (list y z)])))]))
  (gen get)
  (let ([y 0] [z 0]) (get)))
(assert! (equal? (with-a-define 4) '(4 8)))
