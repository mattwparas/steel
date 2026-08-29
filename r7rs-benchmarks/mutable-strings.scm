;;; Mutable strings for the R7RS benchmarks.
;;;
;;; Steel's strings are immutable, so benchmarks using `string-set!` cannot run
;;; against them. `make-string` here returns a mutable char vector wrapped in a
;;; struct; every other string operation dispatches, so native strings -- what
;;; literals, `read` and `symbol->string` still produce -- keep working.
;;;
;;; In practice every `string-set!` in these benchmarks targets a `make-string`
;;; result, so only that one constructor needs to produce the mutable form.
;;;
;;; Caveat for anyone reading benchmark numbers: a benchmark that requires this
;;; module measures the shim, not Steel's native strings.

(require "mstring-struct.scm")
(require "mpair-struct.scm")
(require "native-io.scm")

(provide make-string
         string-set!
         string-ref
         string-length
         string-append
         string=?
         string<?
         string-ci=?
         substring
         string->symbol
         string->number
         string->list
         list->string
         string?
         display
         write
         write-string
         mstring->string)

(define (make-string n [fill #\space])
  (mstring (make-vector n fill)))

(define (string-set! s i c)
  (vector-set! (mstring-chars s) i c))

(define (string-ref s i)
  (if (mstring? s) (vector-ref (mstring-chars s) i) (#%prim.string-ref s i)))

(define (string-length s)
  (if (mstring? s) (vector-length (mstring-chars s)) (#%prim.string-length s)))

(define (string? s)
  (or (mstring? s) (#%prim.string? s)))

;;; Collapse to a native string, which is what every primitive below expects.
(define (mstring->string s)
  (if (mstring? s)
      (#%prim.apply #%prim.string-append (%map %char->string (mutable-vector->list (mstring-chars s))))
      s))

;;; A benchmark may hand these a list built from mutable pairs, so walk lists
;;; with accessors that understand both representations rather than the
;;; native `map`.
(define (%char->string c)
  (#%prim.string c))

(define (%map f xs)
  (cond
    [(#%prim.null? xs) '()]
    [(mpair? xs) (#%prim.cons (f (mpair-mcar xs)) (%map f (mpair-mcdr xs)))]
    [else (#%prim.cons (f (#%prim.car xs)) (%map f (#%prim.cdr xs)))]))

(define (string-append . args)
  (#%prim.apply #%prim.string-append (%map mstring->string args)))

(define (substring s start [end (string-length s)])
  (#%prim.substring (mstring->string s) start end))

(define (string=? a b . rest)
  (#%prim.apply #%prim.string=? (%map mstring->string (#%prim.cons a (#%prim.cons b rest)))))

(define (string<? a b . rest)
  (#%prim.apply #%prim.string<? (%map mstring->string (#%prim.cons a (#%prim.cons b rest)))))

(define (string-ci=? a b)
  (#%prim.string=? (#%prim.string-downcase (mstring->string a))
                   (#%prim.string-downcase (mstring->string b))))

(define (string->symbol s)
  (#%prim.string->symbol (mstring->string s)))

(define (string->number s . rest)
  (#%prim.apply #%prim.string->number (#%prim.cons (mstring->string s) rest)))

;; Benchmarks mutate the result, so hand back the mutable representation.
(define (%list->mlist xs)
  (if (#%prim.null? xs) '() (mpair (#%prim.car xs) (%list->mlist (#%prim.cdr xs)))))

(define (string->list s . rest)
  (%list->mlist
   (#%prim.apply #%prim.string->list (#%prim.cons (mstring->string s) rest))))

(define (list->string chars)
  (#%prim.apply #%prim.string-append (%map %char->string chars)))

(define (display x . rest)
  (#%prim.apply native-display (#%prim.cons (mstring->string x) rest)))

(define (write x . rest)
  (#%prim.apply native-write (#%prim.cons (mstring->string x) rest)))

(define (write-string x . rest)
  (#%prim.apply native-write-string (#%prim.cons (mstring->string x) rest)))
