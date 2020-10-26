; (export
;  caar
;  cadr
;  cdar
;  cddr
;  caaar
;  caadr
;  cadar
;  caddr
;  cdaar
;  cdadr
;  cddar
;  cdddr
;  caaaar
;  caaadr
;  caadar
;  caaddr
;  cadaar
;  cadadr
;  caddar
;  cadddr
;  cdaaar
;  cdaadr
;  cdadar
;  cdaddr
;  cddaar
;  cddadr
;  cdddar
;  cddddr
;  id
;  flip
;  curry
;  curry2
;  compose
;  not
;  foldl
;  map
;  foldr
;  unfold
;  fold
;  reduce
;  max
;  min
;  empty?
;  length
;  mem-helper
;  member
;  assq
;  assoc
;  filter
;  fact)

; (export even?
;         odd?
;         sum
;         add1
;         sub1
;         zero?
;         take
;         drop
;         slice
;         displayln)

; (export or
;         and
;         when
;         unless
;         cond
;         while
;         f>
;         ->
;         ->>
;         l>
;         swap
;         let*
;         letrec*
;         letrec*-helper)

(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing)
    ((quasiquote ((unquote x) xs ...))          (cons x (quasiquote (xs ...))))
    ((quasiquote ((unquote-splicing x)))        (append (list x) (quote ())))
    ((quasiquote ((unquote-splicing x) xs ...)) (append x (quasiquote (xs ...))))
    ((quasiquote (unquote x))                 x)
    ((quasiquote (x))                          (quote (x)))
    ((quasiquote (x xs ...))                   (cons (quasiquote x) (quasiquote (xs ...))))
    ((quasiquote x)                           (quote x))))

(define-syntax lambda-hash
  (syntax-rules ()
    [(lambda-hash (x ...)) (lambda (v) (x ... v))]))


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

(define caar (lambda (pair) (car (car pair))))
(define cadr (lambda (pair) (car (cdr pair))))
(define cdar (lambda (pair) (cdr (car pair))))
(define cddr (lambda (pair) (cdr (cdr pair))))
(define caaar (lambda (pair) (car (car (car pair)))))
(define caadr (lambda (pair) (car (car (cdr pair)))))
(define cadar (lambda (pair) (car (cdr (car pair)))))
(define caddr (lambda (pair) (car (cdr (cdr pair)))))
(define cdaar (lambda (pair) (cdr (car (car pair)))))
(define cdadr (lambda (pair) (cdr (car (cdr pair)))))
(define cddar (lambda (pair) (cdr (cdr (car pair)))))
(define cdddr (lambda (pair) (cdr (cdr (cdr pair)))))
(define caaaar (lambda (pair) (car (car (car (car pair))))))
(define caaadr (lambda (pair) (car (car (car (cdr pair))))))
(define caadar (lambda (pair) (car (car (cdr (car pair))))))
(define caaddr (lambda (pair) (car (car (cdr (cdr pair))))))
(define cadaar (lambda (pair) (car (cdr (car (car pair))))))
(define cadadr (lambda (pair) (car (cdr (car (cdr pair))))))
(define caddar (lambda (pair) (car (cdr (cdr (car pair))))))
(define cadddr (lambda (pair) (car (cdr (cdr (cdr pair))))))
(define cdaaar (lambda (pair) (cdr (car (car (car pair))))))
(define cdaadr (lambda (pair) (cdr (car (car (cdr pair))))))
(define cdadar (lambda (pair) (cdr (car (cdr (car pair))))))
(define cdaddr (lambda (pair) (cdr (car (cdr (cdr pair))))))
(define cddaar (lambda (pair) (cdr (cdr (car (car pair))))))
(define cddadr (lambda (pair) (cdr (cdr (car (cdr pair))))))
(define cdddar (lambda (pair) (cdr (cdr (cdr (car pair))))))
(define cddddr (lambda (pair) (cdr (cdr (cdr (cdr pair))))))
(define id (lambda (obj) obj))
(define flip (lambda (func) (lambda (arg1 arg2) (func arg2 arg1))))
(define curry (lambda (func arg1) (lambda (arg) (func arg1 arg))))
(define curry2 (lambda (func arg1) (lambda (arg2 arg3) (func arg1 arg2 arg3))))
; (define compose (lambda (f g) (lambda (arg) (f (g arg)))))


(define (not a)
  (if a
      #f
      #t))

(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func
             (func (car lst) accum) ; here's the change
             (cdr lst))))


; (define (map func lst)
;    (foldl (lambda (ele acc)
;             (cons (func ele) acc))
;           '()
;           lst))

(define (map func lst)
  (if (empty? lst)
      '()
      (map' func lst)))

; (define (map func lst) (map' func lst))

; (define (map func lst) 
;   (if (empty? lst)
;       lst
;       (mapR func lst)))


(define foldr (lambda (func accum lst)
                (if (null? lst)
                    accum
                    (func (car lst) (foldr func accum (cdr lst))))))



(define unfold (lambda (func init pred)
                 (if (pred init)
                     (cons init '())
                     (cons init (unfold func (func init) pred)))))

(define fold (lambda (f a l) (foldl f a l)))
(define reduce (lambda (f a l) (fold f a l)))
(define max (lambda (x  num-list) (fold (lambda (y z) (if (> y z) y z)) x (cons 0 num-list))))
(define min (lambda (x  num-list) (fold (lambda (y z) (if (< y z) y z)) x (cons 536870911 num-list))))

(define empty? null?)

(define (length lst)
  (define (length-helper lst accum)
    (if (empty? lst)
        accum
        (length-helper (cdr lst) (add1 accum))))
  (length-helper lst 0))

(define mem-helper (lambda (pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc))))
;; (define memq (lambda (obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst)))
;; (define memv (lambda (obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst)))
(define member (lambda (obj lst)     (fold (mem-helper (curry equal? obj) id) #f lst)))
(define assq (lambda (obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist)))
;; (define assv (lambda (obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist)))
; (define assoc (lambda (obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist)))

; (define assoc )

(define (assoc thing alist)
   (if (null? alist)
       #f
       (if (equal? (car (car alist)) thing)
           (car alist)
           (assoc thing (cdr alist)))))


; (define filter (lambda (pred lst)   (foldl (lambda (x y) (if (pred x) (cons x y) y)) '() lst)))

; (define (filter pred lst) (if (empty? lst) lst (filterR pred lst)))

; (define (filter f lst)
;   (define (iter lst result)
;     (cond
;       ((null? lst) result) ;; should reverse here
;       ((f (car lst)) (iter (cdr lst)
;                            (cons (car lst) result)))
;       (else (iter (cdr lst)
;                   result))))
;   (iter lst '()))

; (define (filter pred lst) (filter' pred lst))

(define (filter pred lst)
  (if (empty? lst)
      '()
      (filter' pred lst)))

(define (fact n)
  (define factorial-tail (lambda (n acc) 
                           (if (= n 0)
                               acc
                               (factorial-tail (- n 1)  (* acc n )))))
  (factorial-tail n 1))

(define even-rec? (lambda (x) (if (= x 0) #t (odd-rec? (- x 1)))))
(define odd-rec?  (lambda (x) (if (= x 0) #f (even-rec? (- x 1)))))

(define sum (lambda (x) (reduce + 0 x)))
;; (define head car)
;; (define tail cdr)
(define (add1 n) (+ 1 n))
(define (sub1 n) (- n 1))
(define (zero? n) (= n 0))

;; currently broken, doesn't work properly
(defn (take lst n)
  (defn (loop x l acc)
    (if (= x 0)
        acc
        (loop (- x 1) (cdr l) (cons (car l) acc))))
  (loop n lst (list)))

(define (drop lst n)
  (define (loop x l)
    (if (zero? x)
        l
        (loop (sub1 x) (cdr l))))
  (loop n lst))

(define (slice l offset n)
  (take (drop l offset) n))

(define (displayln object) 
  (display object)
  (newline))


;;; Macros go here:
