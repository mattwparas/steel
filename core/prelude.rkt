(provide
    caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caadar caaddr cadaar 
    cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr id flip curry curry2
    not foldl map foldr unfold fold reduce max min empty? mem-helper member assoc filter even-rec? 
    odd-rec? sum add1 sub1 zero? take drop slice)

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


(define (map func lst)
  (if (null? lst) 
      '() 
      (transduce lst (mapping func) (into-list))))


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

(define mem-helper (lambda (pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc))))
;; (define memq (lambda (obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst)))
;; (define memv (lambda (obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst)))
(define member (lambda (obj lst)     (fold (mem-helper (curry equal? obj) id) #f lst)))

;; TODO come back to this
; (define assq (lambda (obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist)))

;; (define assv (lambda (obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist)))
; (define assoc (lambda (obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist)))

; (define assoc )

(define (assoc thing alist)
   (if (null? alist)
       #f
       (if (equal? (car (car alist)) thing)
           (car alist)
           (assoc thing (cdr alist)))))


(define (filter pred lst)
  (if (empty? lst) 
      '() 
      (transduce lst (filtering pred) (into-list))))

; (define (fact n)
;   (define factorial-tail (lambda (n acc) 
;                            (if (= n 0)
;                                acc
;                                (factorial-tail (- n 1)  (* acc n )))))
;   (factorial-tail n 1))

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