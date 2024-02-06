;; Compatible layer for R7RS Mutable pairs.

; (require "steel/result")

(struct mcons (mcar mcdr)
  #:mutable
  #:printer (lambda (obj printer)
              (if (mlist? obj)

                  (begin
                    (simple-display "'")
                    (printer (mcons->list obj)))

                  (begin
                    (simple-display "'(")
                    (printer (mcons-mcar obj))
                    (simple-display " . ")
                    (printer (mcons-mcdr obj))
                    (simple-display ")")))))

(define set-car! set-mcons-mcar!)
(define set-cdr! set-mcons-mcdr!)

;; Mutable cons!
(define (mcons->list mutable-cons)

  (define (loop mutable-cons builder)

    (if (not (mcons? (dbg! (mcons-mcdr mutable-cons))))

        (#%prim.cons (mcons-mcar mutable-cons) builder)

        (loop (mcons-mcdr mutable-cons) (#%prim.cons (mcons-mcar mutable-cons) builder))))

  (reverse (loop mutable-cons '())))

(define (mlist? cell)
  (define next (mcons-mcdr cell))

  (or (mcons? next) (null? next)))

(define (pair? x)
  (or (mcons? x) (#%prim.pair? x)))

(define (cons a b #:mutable [mutable #false])
  (cond
    [mutable (mcons a b)]
    [(list? b) (#%prim.cons a b)]
    [(mcons? b) (mcons a b)]
    [else (#%prim.cons a b)]))

(define (car a)
  (if (mcons? a) (mcons-mcar a) (#%prim.car a)))

(define (cdr a)
  (if (mcons? a) (mcons-mcdr a) (#%prim.cdr a)))

; (cons 10 20 #:mutable #true)

;; Can make a loop, and garbage collection solves it!
; (define (loop)

;   (define my-cons (mcons 10 (mcons 20 (mcons 30 void))))

;   ;; Make a cycle!
;   (set-car! my-cons my-cons)

;   (loop))

;; TODO: Fix this error - probably just need to not roll back, and when encountering a new value,
;; just fill the open slot / skip the slot in the global environment / fill it with a poisoned
;; value.
;  > :load cogs/collections/mpair.scm
; => '(10 . 20)
; λ (cogs/collections/mpair.scm) > (cons 10 20 #:mutable)
; Unable to locate source and span information for this error: Error: ArityMismatch: Missing keyword argument!
; λ (cogs/collections/mpair.scm) > (cons 10 20 #:mutable #true)
; => '(10 . 20)
; λ (cogs/collections/mpair.scm) > (define my-pair (cons 10 20 #:mutable))
; Unable to locate source and span information for this error: Error: ArityMismatch: Missing keyword argument!
; λ (cogs/collections/mpair.scm) > (define my-pair (cons 10 20 #:mutable #true))
; thread 'main' panicked at 'assertion failed: `(left == right)`
;   left: `999`,
;  right: `1000`', crates/steel-core/src/env.rs:59:13
; note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
