(provide dynamic-wind
         (for-syntax parameterize)
         call/cc
         call-with-current-continuation
         make-parameter
         continuation?

         (for-syntax with-capability)
         #%wrap-with-capability)

;;;;;; Parameters ;;;;;

(struct Parameter (getter value)
  #:mutable
  #:printer (lambda (obj printer-function) (simple-display "<procedure:parameter-procedure>"))
  #:prop:procedure 0)

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

    [(parameterize ([var val] rest ...)
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
  (#%raw-write-string x (current-output-port)))

(define write-string #%raw-write-string)

(define newline
  (case-lambda
    [() (#%raw-write-char #\newline (current-output-port))]
    [(port) (#%raw-write-char #\newline port)]))

(define (simple-displayln x)
  (simple-display x)
  (newline))

(define write-char #%raw-write-char)

(define write #%raw-write)

;;;;;;;;;;;;;;;;;;;;; Port functions ;;;;;;;;;;;;;;;;;;;;;

(provide call-with-output-string
         with-output-to-string)

(define (call-with-output-string proc)
  (define output-string (open-output-string))
  (proc output-string)
  (get-output-string output-string))

(define (with-output-to-string proc)
  (call-with-output-string (lambda (p)
                             (parameterize ([current-output-port p])
                               (proc)))))

;;;;;;;;;;;;;;;;;;;;; Dynamic Wind ;;;;;;;;;;;;;;;;;;;;;;;

(define winders '())

; (define list-tail drop)

; (struct Pair (left right))

(define common-tail
  (lambda (x y)
    (let ([lx (length x)] [ly (length y)])
      (let loop ([x (if (> lx ly) (list-tail x (- lx ly)) x)]
                 [y (if (> ly lx) (list-tail y (- ly lx)) y)])

        (if (equal? x y) x (loop (cdr x) (cdr y)))))))

(define do-wind
  (lambda (new)
    (let ([tail (common-tail new winders)])
      (let f ([ls winders])
        (when (not (equal? ls tail))
          (begin
            ;; TODO: This is probably wrong!
            ; (displayln "setting winders first" ls)
            (set! winders (cdr ls))
            ((cdr (car ls)))
            (f (cdr ls)))))
      (let f ([ls new])
        (when (not (equal? ls tail))
          (begin
            ; (displayln "setting winders second" ls)
            (f (cdr ls))
            ((car (car ls)))
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
    (set! winders (cons (cons in out) winders))
    (let ([ans* (call-with-exception-handler (lambda (err)
                                               ;; Catch the exception on the way out

                                               ; (displayln winders)

                                               ; (displayln "catching exception here")

                                               (set! winders (cdr winders))
                                               (out)
                                               (raise-error err)

                                               void)
                                             (lambda () (body)))])

      ; (displayln winders)

      (set! winders (cdr winders))
      (out)
      ans*)))

;; Use this during macro expansion to make sure that things
;; get wrapped in the proper capabilities
(define-syntax with-capability
  (syntax-rules ()
    [(_ capability-expr guarded-expr)
     ; (let ([evaluated-capability capability-expr])
     (dynamic-wind (lambda () (#%push-capability capability-expr))
                   (lambda () guarded-expr)
                   (lambda () (#%pop-capability)))]))

(define-syntax #%with-capabilities
  (syntax-rules ()
    [(_ guarded-expr capabilities ...)
     (let ([evaluated-capabilities (list capabilities ...)])
       (dynamic-wind (lambda () (apply #%push-capabilities evaluated-capabilities))
                     (lambda () guarded-expr)
                     (lambda () (#%pop-n-capabilities (length evaluated-capabilities)))))]))

;; Wrap the function with a given capability.
;; At the moment, this does not attempt to specialize the arity, however
;; we definitely should because otherwise this becomes rather slow
(define (#%wrap-with-capability capability maybe-function)
  (if (function? maybe-function)
      (lambda args (with-capability capability (apply maybe-function args)))
      maybe-function))
