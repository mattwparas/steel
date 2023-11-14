(provide dynamic-wind
         (for-syntax parameterize)
         call/cc
         call-with-current-continuation
         make-parameter
         continuation?)

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
         simple-display
         simple-displayln
         newline
         write-char
         write)

(define current-input-port (make-parameter (#%default-input-port)))
(define current-output-port (make-parameter (#%default-output-port)))

(define (simple-display x)
  (raw-write-string (current-output-port) x))

(define newline
  (case-lambda
    [() (raw-write-char (current-output-port) #\newline)]
    [(port) (raw-write-char port #\newline)]))

(define (simple-displayln x)
  (simple-display x)
  (newline))

;; TODO: Swap argument order of primitive
(define (write-char char port)
  (raw-write-char port char))

(define (write obj port)
  (raw-write port obj))

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

(define list-tail drop)

(struct Pair (left right))

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
