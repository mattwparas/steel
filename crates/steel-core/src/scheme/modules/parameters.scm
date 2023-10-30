(provide dynamic-wind
         (for-syntax parameterize)
         call/cc
         call-with-current-continuation
         make-parameter
         continuation?)

(define winders '())

(define list-tail drop)

(struct Pair (left right))

(define common-tail
  (lambda (x y)
    (let ([lx (length x)] [ly (length y)])
      (let loop ([x (if (> lx ly) (list-tail x (- lx ly)) x)]
                 [y (if (> ly lx) (list-tail y (- ly lx)) y)])

        ; (displayln x y)
        ; (displayln (equal? x y))
        (if (equal? x y) x (loop (cdr x) (cdr y)))))))

(define do-wind
  (lambda (new)
    (let ([tail (common-tail new winders)])
      (let f ([ls winders])
        (when (not (equal? ls tail))
          (begin
            ;; TODO: This is probably wrong!
            ; (displayln "FIRST" ls)
            (set! winders (cdr ls))
            ((Pair-right (car ls)))
            (f (cdr ls)))))
      (let f ([ls new])
        (when (not (equal? ls tail))
          (begin
            ; (displayln "SECOND" ls)
            (f (cdr ls))
            ((Pair-left (car ls)))
            (set! winders ls)))))))

(struct Continuation (func)
  #:prop:procedure 0
  #:printer (lambda (obj printer)

              (simple-display "#<procedure>")))

(define call/cc
  (lambda (f)
    (#%prim.call/cc (lambda (k)
                      (f (let ([save winders])
                           (Continuation (lambda (x)
                                           (unless (eq? save winders)
                                             (do-wind save))
                                           (k x)))))))))

(define call-with-current-continuation call/cc)

(define (continuation? x)
  (or (Continuation? x) (#%prim.continuation? x)))

(define dynamic-wind
  (lambda (in body out)
    (in)
    (set! winders (cons (Pair in out) winders))

    (let ([ans* (call-with-exception-handler (lambda (err)
                                               ;; Catch the exception on the way out
                                               (set! winders (cdr winders))
                                               (out)
                                               (raise-error err)

                                               void)
                                             (lambda () (body)))])
      (set! winders (cdr winders))
      (out)
      ans*)))

;; TODO: Move these to the tests
; (let ([path '()] [c #f])
;   (let ([add (lambda (s) (set! path (cons s path)))])
;     (dynamic-wind (lambda () (add 'connect))
;                   (lambda ()
;                     (add (call/cc (lambda (c0)
;                                     (set! c c0)
;                                     'talk1))))
;                   (lambda () (add 'disconnect)))
;     (if (< (length path) 4) (c 'talk2) (reverse path))))

; (let ()
;   (dynamic-wind (lambda () (displayln "PRE"))
;                 (lambda ()
;                   (let ()

;                     (dynamic-wind (lambda () (displayln "PRE"))
;                                   (lambda () (displayln "INNER"))
;                                   (lambda () (displayln "POST")))

;                     (displayln "HELLO WORLD!")))
;                 (lambda () (displayln "POST")))

;   (displayln "HELLO WORLD!"))

(struct Parameter (getter value)
  #:mutable
  #:printer (lambda (obj printer-function) (printer-function "<procedure:parameter-procedure>"))
  #:prop:procedure 0)

(define (make-parameter value)
  (define param (Parameter 'uninitialized value))

  (set-Parameter-getter! param (lambda () (Parameter-value param)))

  param)

(define-syntax parameterize
  (syntax-rules ()
    [(parameterize ()
       body ...)
     (begin
       body ...)]

    [(parameterize ([var val] rest ...)
       body ...)

     (let ([old-value (var)])

       (dynamic-wind (lambda () (set-Parameter-value! var val))
                     (lambda ()
                       (parameterize (rest ...)
                         body ...))
                     (lambda () (set-Parameter-value! var old-value))))]))
