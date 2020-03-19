pub const PRELUDE: &str = "
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
(define compose (lambda (f g) (lambda (arg) (f (g arg)))))


(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func
             (func (car lst) accum) ; here's the change
             (cdr lst))))

(define (map func lst)
  (foldl (lambda (ele acc) (push (func ele) acc))
          '()
          lst))


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
(define length (lambda (lst)        (fold (lambda (x y) (+ x 1)) 0 lst)))

;; (define append (lambda (lst lsts)  (foldl (flip (curry2 foldr cons)) lst lsts))) ;; TODO fix

(define (append xs ys)
  (foldr cons-pair ys xs))

;(define (append lhs rhs)
;  (if (null? lhs)
;      rhs
;      (cons (first lhs) (append (rest lhs) rhs))))



(define (reverse lst)
  (begin 
    (define (go lst tail)
    (if (null? lst)
        tail
        (go (cdr lst) (cons (car lst) tail))))
  (go lst '() )))


(define mem-helper (lambda (pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc))))
;; (define memq (lambda (obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst)))
;; (define memv (lambda (obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst)))
(define member (lambda (obj lst)     (fold (mem-helper (curry equal? obj) id) #f lst)))
;; (define assq (lambda (obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist)))
;; (define assv (lambda (obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist)))
(define assoc (lambda (obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist)))


(define filter (lambda (pred lst)   (foldl (lambda (x y) (if (pred x) (push x y) y)) '() lst)))

(define (fact n)
  (begin
    (define factorial-tail (lambda (n acc) 
        (if (= n 0) 
            acc
            (factorial-tail (- n 1)  (* acc n )))))
    (factorial-tail n 1)))

(define even? (lambda (x) (if (= x 0) #t (odd? (- x 1)))))
(define odd?  (lambda (x) (if (= x 0) #f (even? (- x 1)))))
(define sum (lambda (x) (reduce + 0 x)))
(define first car)
(define rest cdr)
(define head car)
(define tail cdr)
(define (add1 n) (+ 1 n))
(define (sub1 n) (- n 1))
(define (zero? n) (= n 0))


(define (take n lst)
  (begin
    (define (loop x l accum)
      (if (or (zero? x) (null? l))
          accum
          (loop (sub1 x) (cdr l) (append accum (list (car l))))))
        (loop n lst '())))

(define (drop n lst)
  (begin
    (define (loop x l)
      (if (zero? x)
        l
        (loop (sub1 x) (cdr l))))
    (loop n lst)))

(define (slice l offset n)
    (take n (drop offset l)))

(define (range l r)
  (begin
    (define (loop l r accum)
    (if (= l r)
        accum
        (loop (add1 l) r (cons l accum))))
  (reverse (loop l r '() ))))

";
