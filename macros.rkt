


(define-syntax for
  (syntax-rules (in)
    [(for element in lst body ...)
     (begin
          (map (lambda (element)
              body ...)
          lst)
          void)]))

(for i in (range 0 10) (display i))


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


;; (define-syntax apply
;;   (syntax-rules ()
;;     [(apply func args ... lst)]))

;; (or #t #f) -> (let ([z x]) ())

(define-syntax or
  (syntax-rules ()
    [(or) #f]
    [(or x) x]
    [(or x y) (let ([z x])
                (if z z y))]
    [(or x y ...) (or x (or y ...))]))

(define-syntax and
  (syntax-rules ()
    [(and) #t]
    [(and x) x]
    [(and x y) (if x y #f)]
    [(and x y ...) (and x (and y ...))]))

(define-syntax when
  (syntax-rules ()
    [(when a b ...)
      (if a (begin b ...) void)]))

(define-syntax unless
  (syntax-rules ()
    [(unless a b ...)
      (if a void (begin b ...))]))

(define-syntax cond
  (syntax-rules (else)
    [(cond [else e1 ...])
      (begin e1 ...)]
    [(cond [e1 e2 ...])
      (when e1 e2 ...)]
    [(cond [e1 e2 ...] c1 ...)
      (if e1
          (begin e2 ...)
          (cond c1 ...))]))

(define-syntax while
  (syntax-rules (do)
    [(while cond do body ...)
      (begin
        (define (loop)
          (when cond
              body ...
              (loop)))
        (loop))]
    [(while cond body ...)
      (begin (define (loop)
              (when cond body ... (loop)))
            (loop))]))

(define-syntax f>
  (syntax-rules ()
    [(f> fun args* ...)
      (lambda (x) (fun x args* ...))]))

(define-syntax ->
  (syntax-rules ()
    [(-> a) a]
    [(-> a (b c ...)) ((f> b c ...) a)]
    [(-> a b c ...) (-> (-> a b) c ...)]))

(define-syntax l>
  (syntax-rules ()
    [(l> fun args* ...)
      (lambda (x) (fun args* ... x))]))

(define-syntax ->>
  (syntax-rules ()
    [(->> a) a]
    [(->> a (b c ...)) ((l> b c ...) a)]
    [(->> a b c ...) (->> (->> a b) c ...)]))

;; TODO
;; (define-syntax compose
;;   (syntax-rules ()
;;     [(compose proc) proc]
;;     [(compose proc1 proc2 ...)

;;      ]

;;     ))
