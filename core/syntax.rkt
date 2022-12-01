(define-syntax steel/base
  (syntax-rules ()
    [(steel/base) 
      (begin
          (require-builtin steel/hash)
          (require-builtin steel/sets)
          (require-builtin steel/lists)
          (require-builtin steel/strings)
          (require-builtin steel/symbols)
          (require-builtin steel/vectors)
          (require-builtin steel/streams)
          (require-builtin steel/contracts)
          (require-builtin steel/identity)
          (require-builtin steel/numbers)
          (require-builtin steel/equality)
          (require-builtin steel/ord)
          (require-builtin steel/transducers)
          (require-builtin steel/io)
          (require-builtin steel/filesystem)
          (require-builtin steel/ports)
          (require-builtin steel/meta)
          (require-builtin steel/json)
          (require-builtin steel/constants)
          (require-builtin steel/syntax))]))

(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing)
    ((quasiquote (unquote x))                         x)
    ((quasiquote ((unquote x) xs ...))          (cons x (quasiquote (xs ...))))
    ((quasiquote ((unquote-splicing x)))        (append x '()))
    ((quasiquote ((unquote-splicing x) xs ...)) (append x (quasiquote (xs ...))))
    ((quasiquote (x xs ...))                   (cons (quasiquote x) (quasiquote (xs ...))))
    ((quasiquote x)                           'x)))

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
  (syntax-rules (else =>)
    [(cond [else => e1 ...])
     (begin e1 ...)]
    [(cond [else e1 ...])
     (begin e1 ...)]
    [(cond [e1 e2 ...])
     (when e1 e2 ...)]
    [(cond [e1 => e2 ...] c1 ...)
     (if e1
         (begin e2 ...)
         (cond c1 ...))]
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

;; TODO add the single argument case
(define-syntax f>
  (syntax-rules ()
    [(f> fun args* ...)
     (lambda (x) (fun x args* ...))]
    [(f> fun) fun]))

(define-syntax ->
  (syntax-rules ()
    [(-> a) a]
    [(-> a (b c ...)) ((f> b c ...) a)]
    [(-> a (b)) ((f> b) a)]
    [(-> a b c ...) (-> (-> a b) c ...)]))

(define-syntax l>
  (syntax-rules ()
    [(l> fun args* ...)
     (lambda (x) (fun args* ... x))]
    [(l> fun) fun]))

(define-syntax ->>
  (syntax-rules ()
    [(->> a) a]
    [(->> a (b c ...)) ((l> b c ...) a)]
    [(->> a (b)) ((l> b) a)]
    [(->> a b c ...) (->> (->> a b) c ...)]))

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

(define-syntax ->/c
  (syntax-rules ()
    [(->/c r)
     (make-function/c (make/c r 'r))]
    [(->/c a b)
     (make-function/c (make/c a 'a) (make/c b 'b))]
    [(->/c a b c)
     (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c))]
    [(->/c a b c d)
     (make-function/c (make/c a 'a) (make/c b 'b)
                      (make/c c 'c) (make/c d 'd))]
    [(->/c a b c d e)
     (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
                      (make/c d 'd) (make/c e 'e))]
    [(->/c a b c d e f)
     (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
                      (make/c d 'd) (make/c e 'e) (make/c f 'f))]
    [(->/c a b c d e f g)
     (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
                      (make/c d 'd) (make/c e 'e) (make/c f 'f)
                      (make/c g 'g))]
    [(->/c a b c d e f g h)
     (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
                      (make/c d 'd) (make/c e 'e) (make/c f 'f)
                      (make/c g 'g) (make/c h 'h))]
    [(->/c a b c d e f g h i)
     (make-function/c (make/c a 'a) (make/c b 'b) (make/c c 'c)
                      (make/c d 'd) (make/c e 'e) (make/c f 'f)
                      (make/c g 'g) (make/c h 'h) (make/c i 'i))]))

;; Macro for basic usage of contracts
(define-syntax define/contract
  (syntax-rules ()
    [(define/contract (name args ...)
       contract
       body ...)
     (define name (bind/c contract (lambda (args ...) body ...) 'name))]
    [(define/contract name contract expr)
     (define name ((bind/c
                      (make-function/c (make/c contract 'contract))
                      (lambda () expr))))]))

(define-syntax module
    (syntax-rules (provide gen-defines contract/out) 
        [(module name (provide ids ...) funcs ...)
         (begin
            (define (datum->syntax name) 
                ((lambda () funcs ... 
                (module provide ids ...))))
            (module gen-defines name ids ...))]
        
        ;; in the contract case, ignore the contract in the hash
        [(module provide (contract/out name contract)) (hash 'name name)]
        ;; Normal case
        [(module provide name) (hash 'name name)]

        ;; in the contract case, ignore the contract in the hash
        [(module provide (contract/out name contract) rest ...)
         (hash-insert (module provide rest ...) 'name name)]

        ;; Normal case
        [(module provide name rest ...)
         (hash-insert (module provide rest ...) 'name name)]

        ;; Module contract provides
        [(module gen-defines mod (contract/out name contract))
         (define (datum->syntax name) (bind/c contract (hash-get mod 'name)))]
        [(module gen-defines mod (contract/out name contract) rest ...)
         (begin (define (datum->syntax name) (bind/c contract (hash-get mod 'name)))
            (module gen-defines mod rest ...))]

        ;; Normal provides
        [(module gen-defines mod name) (define (datum->syntax name) (hash-get mod 'name))]
        [(module gen-defines mod name rest ...)
         (begin (define (datum->syntax name) (hash-get mod 'name))
            (module gen-defines mod rest ...))]))

