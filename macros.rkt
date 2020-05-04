


(define-syntax for
  (syntax-rules (in as)
    [(for element in lst body ...)
     (begin
          (map (lambda (element)
              body ...)
          lst)
          void)]))

;; (for i in (range 0 10) (display i))

(define-syntax swap
  (syntax-rules ()
    [(swap a b)
     (let ([tmp b])
       (begin
         (set! b a)
         (set! a tmp)))]))

(define-syntax let*
  (syntax-rules ()
    ((let* () body ...) ; base case
      ((lambda () body ...)))
    ((let* ((var val) rest ...) body ...) ; binding case
     ((lambda (var) (let* (rest ...) body ...)) val))))

(define-syntax letrec*-helper
  (syntax-rules ()
    ((letrec*-helper () body ...)
      (begin body ...))
    ((letrec*-helper ((var val) rest ...) body ...)
      (begin
        (define var val)
        (letrec*-helper (rest ...) body ...)))))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec* bindings body ...)
      ((lambda ()
        (letrec*-helper bindings body ...))))))

(displayln (let* ((x 1)
                  (y (+ x 1)))
             (+ x y)))

(displayln (letrec* ((x (lambda (n)
                          (if (> n 3) n (x (+ n 1)))))
                     (y 1))
                    (x y)))

(define-syntax map-test
  (syntax-rules ()
    [(map-test (x ...) lst)
     (map (lambda (v) (x ... v)) lst)]))

(displayln (map-test (* 2) (list 1 2 3 4)))

(define-syntax filter-test
  (syntax-rules ()
    [(filter-test (x ...) lst)
     (filter (lambda (v) (x ... v)) lst)]))

(displayln (filter-test (= 2) (list 1 2 3 4 5 2 4 2)))
