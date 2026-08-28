;;; Mutable pairs for the R7RS benchmarks.
;;;
;;; Steel's lists are immutable, so benchmarks that use `set-car!` / `set-cdr!`
;;; cannot run against native pairs. This module rebuilds the list surface on
;;; top of a mutable struct: `cons` (and everything that builds a list) yields
;;; an `mpair`, and every accessor dispatches so that native lists -- which is
;;; what quoted literals and `read` still produce -- keep working unchanged.
;;;
;;; Requiring this module shadows the list primitives for that file only.
;;;
;;; Caveat for anyone reading benchmark numbers: pairs here are heap structs
;;; behind a dispatching accessor, so a ported benchmark measures this shim and
;;; is NOT comparable to a benchmark running on Steel's native lists.

(provide cons
         car
         cdr
         set-car!
         set-cdr!
         pair?
         list
         list?
         length
         append
         reverse
         list-ref
         list-tail
         list-copy
         list->vector
         vector->list
         list->string
         map
         for-each
         assq
         assv
         assoc
         memq
         memv
         member
         apply
         equal?
         cons*
         last-pair
         caar
         cadr
         cdar
         cddr
         caaar
         caadr
         cadar
         caddr
         cdaar
         cdadr
         cddar
         cdddr
         caaaar
         caaadr
         caadar
         caaddr
         cadaar
         cadadr
         caddar
         cadddr
         cdaaar
         cdaadr
         cdadar
         cdaddr
         cddaar
         cddadr
         cdddar
         cddddr
         mlist->list
         list->mlist
         deep-list->mlist
         read
         vector)

(require "mpair-struct.scm")
(require "native-read.scm")

(define set-car! set-mpair-mcar!)
(define set-cdr! set-mpair-mcdr!)

(define (cons a b)
  (mpair a b))

(define (car p)
  (if (mpair? p) (mpair-mcar p) (#%prim.car p)))

(define (cdr p)
  (if (mpair? p) (mpair-mcdr p) (#%prim.cdr p)))

(define (pair? x)
  (or (mpair? x) (#%prim.pair? x)))

;;; Conversions between the two representations. Quoted literals, `read`
;;; results and vector contents arrive as native lists; benchmark results
;;; have to be compared against them.

(define (list->mlist xs)
  (if (#%prim.null? xs) '() (mpair (#%prim.car xs) (list->mlist (#%prim.cdr xs)))))

;;; Benchmarks mutate the program they read in, so `read` has to hand back the
;;; mutable representation -- a native list from the reader cannot be
;;; `set-car!`ed.
(define (deep-list->mlist x)
  (cond
    [(#%prim.null? x) '()]
    [(#%prim.pair? x) (mpair (deep-list->mlist (#%prim.car x)) (deep-list->mlist (#%prim.cdr x)))]
    [else x]))

(define (read . port)
  (deep-list->mlist (#%prim.apply native-read port)))

(define (mlist->list xs)
  (cond
    [(#%prim.null? xs) '()]
    [(mpair? xs) (#%prim.cons (mlist->list (mpair-mcar xs)) (mlist->list (mpair-mcdr xs)))]
    [(#%prim.pair? xs) (#%prim.cons (mlist->list (#%prim.car xs)) (mlist->list (#%prim.cdr xs)))]
    [else xs]))

(define (list . args)
  (list->mlist args))

(define (list? x)
  (cond
    [(#%prim.null? x) #t]
    [(mpair? x) (list? (mpair-mcdr x))]
    [else (#%prim.list? x)]))

;; A chain can be mixed: `(cons x '(1 2))` yields an mpair whose cdr is a
;; native list, so walk it through the dispatching `cdr`.
(define (length xs)
  (if (mpair? xs)
      (let loop ([xs xs] [n 0])
        (if (#%prim.null? xs) n (loop (cdr xs) (+ n 1))))
      (#%prim.length xs)))

(define (append . lists)
  (cond
    [(#%prim.null? lists) '()]
    [(#%prim.null? (#%prim.cdr lists)) (#%prim.car lists)]
    [else
     (let loop ([xs (#%prim.car lists)])
       (if (or (#%prim.null? xs) (not (pair? xs)))
           (apply append (#%prim.cdr lists))
           (mpair (car xs) (loop (cdr xs)))))]))

(define (reverse xs)
  (let loop ([xs xs] [acc '()])
    (if (#%prim.null? xs) acc (loop (cdr xs) (mpair (car xs) acc)))))

(define (list-tail xs k)
  (if (= k 0) xs (list-tail (cdr xs) (- k 1))))

(define (list-ref xs k)
  (car (list-tail xs k)))

(define (list-copy xs)
  (if (pair? xs) (mpair (car xs) (list-copy (cdr xs))) xs))

(define (last-pair xs)
  (if (pair? (cdr xs)) (last-pair (cdr xs)) xs))

(define (cons* . args)
  (let loop ([args args])
    (if (#%prim.null? (#%prim.cdr args))
        (#%prim.car args)
        (mpair (#%prim.car args) (loop (#%prim.cdr args))))))

;;; R7RS vectors are mutable; Steel's `vector` builds an immutable one, so
;;; benchmarks that `vector-set!` a vector they built need this instead.
(define (vector . args)
  (let* ([n (#%prim.length args)]
         [v (make-vector n 0)])
    (let loop ([i 0] [xs args])
      (if (#%prim.null? xs)
          v
          (begin
            (vector-set! v i (#%prim.car xs))
            (loop (+ i 1) (#%prim.cdr xs)))))))

(define (list->vector xs)
  (#%prim.apply vector (%native-list xs)))

(define (vector->list v)
  (list->mlist (mutable-vector->list v)))

(define (list->string xs)
  (#%prim.apply string-append (%native-list (map (lambda (c) (string c)) xs))))

;;; Flatten one level of an mpair chain into a native list, for handing to
;;; primitives that only understand native lists (apply, vector, ...).
(define (%native-list xs)
  (let loop ([xs xs] [acc '()])
    (if (#%prim.null? xs)
        (#%prim.reverse acc)
        (loop (cdr xs) (#%prim.cons (car xs) acc)))))

(define (apply f . args)
  ;; (apply f a b ... rest)
  (let loop ([args args] [acc '()])
    (if (#%prim.null? (#%prim.cdr args))
        (#%prim.apply f (#%prim.append (#%prim.reverse acc) (%native-list (#%prim.car args))))
        (loop (#%prim.cdr args) (#%prim.cons (#%prim.car args) acc)))))

(define (map f xs . rest)
  (if (#%prim.null? rest)
      (let loop ([xs xs])
        (if (or (#%prim.null? xs) (not (pair? xs))) '() (mpair (f (car xs)) (loop (cdr xs)))))
      (let loop ([lists (#%prim.cons xs rest)])
        (if (%any-exhausted? lists)
            '()
            (mpair (#%prim.apply f (%heads lists)) (loop (%tails lists)))))))

(define (for-each f xs . rest)
  (if (#%prim.null? rest)
      (let loop ([xs xs])
        (if (or (#%prim.null? xs) (not (pair? xs)))
            void
            (begin
              (f (car xs))
              (loop (cdr xs)))))
      (let loop ([lists (#%prim.cons xs rest)])
        (if (%any-exhausted? lists)
            void
            (begin
              (#%prim.apply f (%heads lists))
              (loop (%tails lists)))))))

(define (%any-exhausted? lists)
  (cond
    [(#%prim.null? lists) #f]
    [(not (pair? (#%prim.car lists))) #t]
    [else (%any-exhausted? (#%prim.cdr lists))]))

(define (%heads lists)
  (if (#%prim.null? lists)
      '()
      (#%prim.cons (car (#%prim.car lists)) (%heads (#%prim.cdr lists)))))

(define (%tails lists)
  (if (#%prim.null? lists)
      '()
      (#%prim.cons (cdr (#%prim.car lists)) (%tails (#%prim.cdr lists)))))

(define (%assoc-by eq key alist)
  (let loop ([alist alist])
    (cond
      [(#%prim.null? alist) #f]
      [(not (pair? alist)) #f]
      [(eq (car (car alist)) key) (car alist)]
      [else (loop (cdr alist))])))

(define (assq key alist)
  (%assoc-by #%prim.eq? key alist))
(define (assv key alist)
  (%assoc-by #%prim.eqv? key alist))
(define (assoc key alist)
  (%assoc-by equal? key alist))

(define (%member-by eq x xs)
  (let loop ([xs xs])
    (cond
      [(#%prim.null? xs) #f]
      [(not (pair? xs)) #f]
      [(eq (car xs) x) xs]
      [else (loop (cdr xs))])))

(define (memq x xs)
  (%member-by #%prim.eq? x xs))
(define (memv x xs)
  (%member-by #%prim.eqv? x xs))
(define (member x xs)
  (%member-by equal? x xs))

;;; Structural equality across the two representations, so a benchmark result
;;; built from mpairs still compares equal to the expected native list read
;;; out of the input file.
(define (equal? a b)
  (cond
    [(and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))]
    [(pair? a) #f]
    [(pair? b) #f]
    [else (#%prim.equal? a b)]))

(define (caar x)
  (car (car x)))
(define (cadr x)
  (car (cdr x)))
(define (cdar x)
  (cdr (car x)))
(define (cddr x)
  (cdr (cdr x)))
(define (caaar x)
  (car (caar x)))
(define (caadr x)
  (car (cadr x)))
(define (cadar x)
  (car (cdar x)))
(define (caddr x)
  (car (cddr x)))
(define (cdaar x)
  (cdr (caar x)))
(define (cdadr x)
  (cdr (cadr x)))
(define (cddar x)
  (cdr (cdar x)))
(define (cdddr x)
  (cdr (cddr x)))
(define (caaaar x)
  (car (caaar x)))
(define (caaadr x)
  (car (caadr x)))
(define (caadar x)
  (car (cadar x)))
(define (caaddr x)
  (car (caddr x)))
(define (cadaar x)
  (car (cdaar x)))
(define (cadadr x)
  (car (cdadr x)))
(define (caddar x)
  (car (cddar x)))
(define (cadddr x)
  (car (cdddr x)))
(define (cdaaar x)
  (cdr (caaar x)))
(define (cdaadr x)
  (cdr (caadr x)))
(define (cdadar x)
  (cdr (cadar x)))
(define (cdaddr x)
  (cdr (caddr x)))
(define (cddaar x)
  (cdr (cdaar x)))
(define (cddadr x)
  (cdr (cdadr x)))
(define (cdddar x)
  (cdr (cddar x)))
(define (cddddr x)
  (cdr (cdddr x)))
