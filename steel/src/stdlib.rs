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

;(define (map func lst)
 ; (reverse (foldl (lambda (ele acc) (cons (func ele) acc))
 ;         '()
 ;         lst)))

(define (map func lst) 
  (if (empty? lst)
      lst
      (map' func lst)))


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
;; (define length (lambda (lst)        (foldl (lambda (x y) (+ x 1)) 0 lst)))

(define empty? null?)

(define (length lst)
  (define (length-helper lst accum)
    (if (empty? lst)
        accum
        (length-helper (cdr lst) (add1 accum))))
  (length-helper lst 0))


;; (define append (lambda (lst lsts)  (foldl (flip (curry2 foldr cons)) lst lsts))) ;; TODO fix

;; (define (append xs ys)
;;   (foldr cons ys xs))

;(define (append lhs rhs)
;  (if (null? lhs)
;      rhs
;      (cons (first lhs) (append (rest lhs) rhs))))



;(define (reverse lst)
;  (begin 
;    (define (go lst tail)
;    (if (null? lst)
;        tail
;        (go (cdr lst) (cons (car lst) tail))))
;  (go lst '() )))


(define mem-helper (lambda (pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc))))
;; (define memq (lambda (obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst)))
;; (define memv (lambda (obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst)))
(define member (lambda (obj lst)     (fold (mem-helper (curry equal? obj) id) #f lst)))
(define assq (lambda (obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist)))
;; (define assv (lambda (obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist)))
(define assoc (lambda (obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist)))


;; (define filter (lambda (pred lst)   (reverse (foldl (lambda (x y) (if (pred x) (cons x y) y)) '() lst))))

(define (filter pred lst) (if (empty? lst) lst (filter' pred lst)))

(define (fact n)
  (define factorial-tail (lambda (n acc) 
      (if (= n 0) 
          acc
          (factorial-tail (- n 1)  (* acc n )))))
  (factorial-tail n 1))

(define even? (lambda (x) (if (= x 0) #t (odd? (- x 1)))))
(define odd?  (lambda (x) (if (= x 0) #f (even? (- x 1)))))
(define sum (lambda (x) (reduce + 0 x)))
(define head car)
(define tail cdr)
(define (add1 n) (+ 1 n))
(define (sub1 n) (- n 1))
(define (zero? n) (= n 0))


(define (take lst n)
  (define (loop x l accum)
    (if (or (zero? x) (null? l))
        accum
        (loop (sub1 x) (cdr l) (append accum (list (car l))))))
    (loop n lst '()))

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

;(define (range l r)
;  (begin
;    (define (loop l r accum)
;    (if (= l r)
;        accum
;        (loop l (sub1 r) (cons r accum))))
;  (loop l r '() )))


;;; Macros go here:

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

";

pub const TRIESORT: &str = "
(struct trie (char children end-word? word-up-to))

;; Rename functions for the sake of compatibility
(define empty (list))
(define empty-trie (trie void empty #f empty))
(define char<? <)
(define char=? =)
(define pair? list?)

;; Throw in a mediocre flatten definition
(define (flatten lst)
  (cond ((null? lst) empty)
        ((pair? lst)
         (append (flatten (car lst)) (flatten (cdr lst))))
        (else (list lst))))

;; contract: (listof char?) (listof tries?) integer? -> (listof trie?)
(define (create-children char-list lst prefix-chars)
  (cond [(= (length char-list) 1)
         (handle-last-letter char-list lst prefix-chars)]
        [else ;; you are in the middle of the word
         (handle-intern-letter char-list lst prefix-chars)]))

;; contract: (listof char?) (listof trie?) integer? -> (listof trie?)
(define (handle-last-letter char-list lst prefix-chars)
  (define char (first char-list))
  (define next-prefix (append prefix-chars (list char)))
  (cond [(empty? lst) ;; children are empty, return list of empty children
         (list (trie char empty #t next-prefix))]
        [(char<? char (trie-char (first lst))) ;; less than, put it to the left
         (cons (trie char empty #t next-prefix) lst)]
        [(char=? char (trie-char (first lst))) ;; equal, step down a level
         (cons (trie char (trie-children (first lst)) #t next-prefix) (rest lst))]
        [else ;; move to the right
         (cons (first lst)
               (create-children char-list (rest lst) prefix-chars))]))

;; contract: (listof char?) (listof trie?) integer? -> (listof trie?)
(define (handle-intern-letter char-list lst prefix-chars)
  (define char (first char-list))
  (define next-prefix (append prefix-chars (list char)))
  (cond [(empty? lst) ;; no children, pop off front and step down
         (list (trie char (create-children
                           (rest char-list) empty next-prefix) #f next-prefix))]
        [(char<? char (trie-char (first lst))) ;; place where it is, pop off front and go
         (cons (trie char (create-children
                           (rest char-list) empty next-prefix) #f next-prefix) lst)]
        [(char=? char (trie-char (first lst))) ;; equal, step down
         (cons (trie char (create-children (rest char-list) (trie-children (first lst)) next-prefix)
                     (trie-end-word? (first lst))
                     (trie-word-up-to (first lst)))
               (rest lst))]
        [else ; move to the right
         (cons (first lst)
               (create-children char-list (rest lst) prefix-chars))]))

;; contract: trie? string? integer? -> trie?
(define (insert root-trie word)
  (define char-list (string->list word))
  (trie
   (trie-char root-trie)
   (create-children char-list (trie-children root-trie) empty)
   (trie-end-word? root-trie)
   (trie-word-up-to root-trie)))

; contract: trie? trie? -> boolean?
(define (trie<? trie-node1 trie-node2)
  (char<? (trie-char trie-node1) (trie-char trie-node2)))

;; contract: trie? -> void
(define (pre-order-traverse trie-node)
  (displayln (list (trie-char trie-node) (trie-end-word? trie-node) (trie-word-up-to trie-node)))
  (map pre-order-traverse (trie-children trie-node))
  \"finished\")

;; contract: trie? (listof string?) -> trie?
(define (build-trie-from-list-of-words trie list-of-words)
  (cond
    [(= (length list-of-words) 1)
     (insert trie (first list-of-words))]
    [else
     (build-trie-from-list-of-words
      (insert trie (first list-of-words))
      (rest list-of-words))]))

;; ------------------ SORTING ---------------------- ;;

(define (trie-sort list-of-words)
  (define new-trie (build-trie-from-list-of-words empty-trie list-of-words))
  (pre-order new-trie))

; THIS ONE WORKS (using con and flatten)
;; contract: trie? -> (listof string?)
(define (pre-order trie-node)
  (if (trie-end-word? trie-node)
    (cons (list->string (trie-word-up-to trie-node))
      (flatten (map pre-order (trie-children trie-node))))
    (flatten (map pre-order (trie-children trie-node)))))
";
