(begin
      (define ___reduced-options___
        (hash))
      (define reduced
        (quote
          unintialized))
      (define reduced?
        (quote
          uninitialized))
      (define reduced-val
        (quote
          uninitialized))
      (define set-reduced-val!
        (quote
          unintialized))
      (let ((prototypes (make-struct-type
             (quote
               reduced)
             2)))
        (let ((constructor-proto (list-ref prototypes 0))
            (predicate-proto (list-ref prototypes 1))
            (getter-proto (list-ref prototypes 2))
            (setter-proto (list-ref prototypes 3)))
          (begin
                (set! reduced
                  (λ (val)
                    (constructor-proto
                       ___reduced-options___
                       val)))
                (set! reduced? predicate-proto)
                (set! reduced-val
                  (λ (this)
                    (getter-proto this 1)))
                (set! set-reduced-val!
                  (λ (this value)
                    (setter-proto this 1 value)))
                void))))

(define unreduce
  reduced-val)

(define ensure-reduced
  (λ (x)
    (if (reduced? x) x (reduced x))))

(define preserving-reduced
  (λ (reducer)
    (λ (a b)
      (let ((return (reducer a b)))
        (if (reduced? return) (reduced return) return)))))

(define list-reduce
  (λ (f identity lst)
    (if (null? lst)
      identity
      (let ((v (f identity (car lst))))
        (if (reduced? v)
          (unreduce v)
          (list-reduce f v (cdr lst)))))))

(begin
      (define ___<Nothing>-options___
        (hash))
      (define <Nothing>
        (quote
          unintialized))
      (define <Nothing>?
        (quote
          uninitialized))
      (let ((prototypes (make-struct-type
             (quote
               <Nothing>)
             1)))
        (let ((constructor-proto (list-ref prototypes 0))
            (predicate-proto (list-ref prototypes 1))
            (getter-proto (list-ref prototypes 2))
            (setter-proto (list-ref prototypes 3)))
          (begin
                (set! <Nothing>
                  (λ ()
                    (constructor-proto
                       ___<Nothing>-options___)))
                (set! <Nothing>? predicate-proto)
                void))))

(define nothing
  (<Nothing>))

(define nothing?
  <Nothing>?)

(define ensure-reduced
  (λ (x)
    (if (reduced? x) x (reduced x))))

(define preserving-reduced
  (λ (f)
    (λ (a b)
      (let ((return (f a b)))
        (if (reduced? return) (reduced return) return)))))

(define rcons
  (λ args
    (displayln "Calling rcons!")
    (let ((l (length args)))
      (if (= l (length (quote ())))
        (apply (λ () (quote ())) args)
        (if (= l (length (quote (lst))))
          (apply (λ (lst) (reverse lst)) args)
          (if (= l (length (quote (lst x))))
            (apply (λ (lst x) (cons x lst)) args)
            (error! "Arity mismatch")))))))

; (define reverse-rcons
;   (λ args
;     (let ((l (length args)))
;       (if (= l (length (quote ())))
;         (apply (λ () (quote ())) args)
;         (if (= l (length (quote (lst))))
;           (apply (λ (lst) lst) args)
;           (if (= l (length (quote (lst x))))
;             (apply (λ (lst x) (cons x lst)) args)
;             (error! "Arity mismatch")))))))

; (define rcount
;   (λ args
;     (let ((l (length args)))
;       (if (= l (length (quote ())))
;         (apply (λ () 0) args)
;         (if (= l (length (quote (result))))
;           (apply (λ (result) result) args)
;           (if (= l (length (quote (result input))))
;             (apply (λ (result input) (+ 1 result)) args)
;             (error! "Arity mismatch")))))))

; (define rany
;   (λ (pred)
;     (λ args
;       (let ((l (length args)))
;         (if (= l (length (quote ())))
;           (apply (λ () #false) args)
;           (if (= l (length (quote (result))))
;             (apply (λ (result) result) args)
;             (if (= l (length (quote (result input))))
;               (apply
;                  (λ (result input)
;                    (let ((test (pred input)))
;                      (if test (reduced test) #false)))
;                  args)
;               (error! "Arity mismatch"))))))))

; (define revery
;   (λ (pred)
;     (λ args
;       (let ((l (length args)))
;         (if (= l (length (quote ())))
;           (apply (λ () #true) args)
;           (if (= l (length (quote (result))))
;             (apply (λ (result) result) args)
;             (if (= l (length (quote (result input))))
;               (apply
;                  (λ (result input)
;                    (let ((test (pred input)))
;                      (if (if result test #false)
;                        test
;                        (reduced #false))))
;                  args)
;               (error! "Arity mismatch"))))))))

(define list-transduce
  (λ args
    (displayln args)
    (let ((l (length args)))
      (if (= l (length (quote (xform f coll))))
        (apply
           (λ (xform f coll)
             (displayln f)
             (displayln (multi-arity? f))
             (list-transduce xform f (f) coll))
           args)
        (if (= l (length (quote (xform f init coll))))
          (apply
             (λ (xform f init coll)
               (let ((xf (xform f)))
                 (let ((result (list-reduce xf init coll)))
                   (xf result))))
             args)
          (error! "Arity mismatch"))))))

(define tmap
  (λ (f)
    (λ (reducer)
      (λ args
        (let ((l (length args)))
          (if (= l (length (quote ())))
            (apply (λ () (reducer)) args)
            (if (= l (length (quote (result))))
              (apply (λ (result) (reducer result)) args)
              (if (= l (length (quote (result input))))
                (apply
                   (λ (result input)
                     (reducer result (f input)))
                   args)
                (error! "Arity mismatch")))))))))
;; λ > 




(list-transduce (tmap (lambda (x) (+ x 1))) rcons (list 0 1 2 3))