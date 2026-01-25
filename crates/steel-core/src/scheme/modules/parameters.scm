(provide dynamic-wind
         (for-syntax parameterize)
         call/cc
         call-with-current-continuation
         make-parameter
         continuation?)

(require-builtin steel/base)

;;;;;; Parameters ;;;;;

; (struct Parameter (getter value)
;   #:mutable
;   #:printer (lambda (obj printer-function) (simple-display "<procedure:parameter-procedure>"))
;   #:prop:procedure 0)

(define struct__doc__
  "
 Syntax:

 Macro for creating a new struct, in the form of:
 `(struct <struct-name> (fields ...) options ...)`
 The options can consist of the following:

 Single variable options (those which their presence indicates #true)
 - #:mutable
 - #:transparent

 Other options must be presented as key value pairs, and will get stored
 in the struct instance. They will also be bound to the variable
 ___<struct-name>-options___ in the same lexical environment where the
 struct was defined. For example:

 (Applesauce (a b c) #:mutable #:transparent #:unrecognized-option 1234)

 Will result in the value `___Applesauce-options___` like so:
 (hash #:mutable #true #:transparent #true #:unrecognized-option 1234)

 By default, structs are immutable, which means setter functions will not
 be generated. Also by default, structs are not transparent, which means
 printing them will result in an opaque struct that does not list the fields

 # Examples

```scheme
(struct apple (size color))

(apple 'small 'red) ;; => (apple)
(define my-apple (apple 'small 'red))

(apple-size my-apple) ;; => 'small
(apple-color my-apple) ;; => 'red

(apple? my-apple) ;; => #true

;; With extra options

(struct apple (size color) #:transparent #:mutable)

(apple 'small 'green) => (apple 'small 'green)

(define my-apple (apple 'small 'green))

(set-apple-color! my-apple 'red)

(apple-color my-apple) ;; => 'red
```

  ")

;; Bootstrapped parameters
(define ___Parameter-options___
  (hash (quote #:mutable)
        #true
        (quote #:name)
        (quote Parameter)
        (quote #:fields)
        (quote (getter value))
        (quote #:prop:procedure)
        0
        (quote #:printer)
        (λ (obj2 printer-function2) (simple-display "<procedure:parameter-procedure>"))
        (quote #:transparent)
        #false))
(define Parameter (quote unintialized))
(define struct:Parameter (quote uninitialized))
(define Parameter? (quote uninitialized))
(define Parameter-getter (quote uninitialized))
(define Parameter-value (quote uninitialized))
(define set-Parameter-getter! (quote unintialized))
(define set-Parameter-value! (quote unintialized))
(%plain-let
 ((prototypes2 (make-struct-type (quote Parameter) 2)))
 (%plain-let
  ((struct-type-descriptor3 (list-ref prototypes2 0)) (constructor-proto3 (list-ref prototypes2 1))
                                                      (predicate-proto3 (list-ref prototypes2 2))
                                                      (getter-proto3 (list-ref prototypes2 3))
                                                      (getter-proto-list3 (list-ref prototypes2 4)))
  (begin
    (set! struct:Parameter struct-type-descriptor3)
    (#%vtable-update-entry! struct-type-descriptor3 0 ___Parameter-options___)
    (set! Parameter (λ (getter4 value4) (constructor-proto3 (#%box getter4) (#%box value4))))
    (set! Parameter? predicate-proto3)
    (set! Parameter-getter (λ (this4) (#%unbox (getter-proto3 this4 0))))
    (set! Parameter-value (λ (this4) (#%unbox (getter-proto3 this4 1))))
    (set! set-Parameter-getter! (λ (this4 value4) (#%set-box! (getter-proto3 this4 0) value4)))
    (set! set-Parameter-value! (λ (this4 value4) (#%set-box! (getter-proto3 this4 1) value4)))
    void)))

(define (make-parameter value)
  (define param (Parameter 'uninitialized value))

  (set-Parameter-getter! param
                         (case-lambda
                           [() (Parameter-value param)]
                           [(new-value) (set-Parameter-value! param new-value)]))

  param)

(define-syntax parameterize
  (syntax-rules ()
    [(parameterize ()
       body ...)
     (begin
       body ...)]

    [(parameterize ([var val]
                    rest ...)
       body ...)

     (let ([old-value (var)])
       (dynamic-wind (lambda () (set-Parameter-value! var val))
                     (lambda ()
                       (parameterize (rest ...)
                         body ...))
                     (lambda () (set-Parameter-value! var old-value))))]))

;;;;;;; Bootstrapping printing functions for various primitive structs

(provide current-input-port
         current-output-port
         current-error-port
         simple-display
         simple-displayln
         newline
         write-char
         write
         write-string)

(define current-input-port (make-parameter (#%default-input-port)))
(define current-output-port (make-parameter (#%default-output-port)))
(define current-error-port (make-parameter (#%default-error-port)))

(define (simple-display x)
  (#%raw-display x (current-output-port)))

(define write-string #%raw-write-string)

(define newline
  (case-lambda
    [() (#%raw-write-char #\newline (current-output-port))]
    [(port) (#%raw-write-char #\newline port)]))

(define (simple-displayln x)
  (simple-display x)
  (newline))

(define write-char #%raw-write-char)

(define write
  (case-lambda
    [(arg) (#%raw-write arg (current-output-port))]
    [(arg port) (#%raw-write arg port)]))

;;;;;;;;;;;;;;;;;;;;; Dynamic Wind ;;;;;;;;;;;;;;;;;;;;;;;

; (define winders '())
(define winders (make-tls '()))

; (define list-tail drop)

; (struct Pair (left right))

(define common-tail
  (lambda (x y)
    (let ([lx (length x)]
          [ly (length y)])
      (let loop ([x (if (> lx ly)
                        (list-tail x (- lx ly))
                        x)]
                 [y (if (> ly lx)
                        (list-tail y (- ly lx))
                        y)])

        (if (equal? x y)
            x
            (loop (cdr x) (cdr y)))))))

(define do-wind
  (lambda (new)
    (let ([tail (common-tail new (get-tls winders))])
      (let f ([ls (get-tls winders)])
        (when (not (equal? ls tail))
          (begin
            ;; TODO: This is probably wrong!
            ; (displayln "setting winders first" ls)
            (set-tls! winders (cdr ls))
            ((cdr (car ls)))
            (f (cdr ls)))))
      (let f ([ls new])
        (when (not (equal? ls tail))
          (begin
            ; (displayln "setting winders second" ls)
            (f (cdr ls))
            ((car (car ls)))
            (set-tls! winders ls)))))))

; (struct Continuation (func)
;   #:prop:procedure 0
;   #:printer (lambda (obj printer) (simple-display "#<procedure>")))

;; Bootstrapped continuation
(define ___Continuation-options___
  (hash (quote #:printer)
        (λ (obj2 printer2) (simple-display "#<procedure>"))
        (quote #:name)
        (quote Continuation)
        (quote #:mutable)
        #false
        (quote #:fields)
        (quote (func))
        (quote #:transparent)
        #false
        (quote #:prop:procedure)
        0))
(define Continuation (quote unintialized))
(define struct:Continuation (quote uninitialized))
(define Continuation? (quote uninitialized))
(define Continuation-func (quote uninitialized))
(%plain-let
 ((prototypes2 (make-struct-type (quote Continuation) 1)))
 (%plain-let
  ((struct-type-descriptor3 (list-ref prototypes2 0)) (constructor-proto3 (list-ref prototypes2 1))
                                                      (predicate-proto3 (list-ref prototypes2 2))
                                                      (getter-proto3 (list-ref prototypes2 3))
                                                      (getter-proto-list3 (list-ref prototypes2 4)))
  (begin
    (set! struct:Continuation struct-type-descriptor3)
    (#%vtable-update-entry! struct-type-descriptor3 0 ___Continuation-options___)
    (set! Continuation constructor-proto3)
    (set! Continuation? predicate-proto3)
    (set! Continuation-func (list-ref getter-proto-list3 0))
    void)))

(define call/cc
  (lambda (f)
    (#%prim.call/cc (lambda (k)
                      (f (let ([save (get-tls winders)])
                           (Continuation (lambda (x)
                                           (unless (eq? save (get-tls winders))
                                             (do-wind save))
                                           (k x)))))))))

(define call-with-current-continuation call/cc)

(define (continuation? x)
  (or (Continuation? x) (#%prim.continuation? x)))

(define dynamic-wind
  (lambda (in body out)
    (in)
    (set-tls! winders (cons (cons in out) (get-tls winders)))
    (let ([ans* (call-with-exception-handler (lambda (err)
                                               ;; Catch the exception on the way out

                                               ; (displayln winders)

                                               ; (displayln "catching exception here")

                                               (set-tls! winders (cdr (get-tls winders)))
                                               (out)
                                               (raise-error err)

                                               void)
                                             (lambda () (body)))])

      ; (displayln winders)

      (set-tls! winders (cdr (get-tls winders)))
      (out)
      ans*)))
