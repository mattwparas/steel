(require-builtin #%private/steel/reader as reader.)
(require "steel/result")
(require "#%private/steel/control")

(provide read
         read-syntax-object)

(define *reader* (reader.new-reader))

(define (read . port)
  (if (null? port)
      (reader.#%intern (read-impl reader.reader-read-one))
      (parameterize ([current-input-port (car port)])
        (reader.#%intern (read-impl reader.reader-read-one)))))

(define (read-syntax-object . port)
  (if (null? port)
      (read-impl reader.reader-read-one-syntax-object)
      (parameterize ([current-input-port (car port)])
        (read-impl reader.reader-read-one-syntax-object))))

(define (read-impl finisher)
  (cond
    [(reader.reader-empty? *reader*)

     (if (or (#%string-input-port? (current-input-port)) (#%file-input-port? (current-input-port)))
         (begin

           (define entry (read-port-to-string (current-input-port)))
           (reader.reader-push-string *reader* entry)

           (let ([next (finisher *reader*)])
             ; (displayln next)
             (if (void? next)
                 (begin
                   (let ([maybe-next-line (read-line-from-port (current-input-port))])
                     (if (eof-object? maybe-next-line)
                         (if (or (#%string-input-port? (current-input-port))
                                 (#%file-input-port? (current-input-port)))
                             maybe-next-line
                             (begin
                               (set! *reader* (reader.new-reader))
                               (error "missing closing paren - unexpected eof")))
                         ;; If the next line is not empty,
                         (begin
                           (reader.reader-push-string *reader* maybe-next-line)
                           (read-impl finisher)))))

                 next)))
         ; next))

         (begin

           (define next-line (read-line-from-port (current-input-port)))
           (cond
             [(string? next-line)
              (reader.reader-push-string *reader* next-line)

              (let ([next (finisher *reader*)])
                (if (void? next)
                    (begin
                      (let ([maybe-next-line (read-line-from-port (current-input-port))])
                        (if (eof-object? maybe-next-line)
                            (if (or (#%string-input-port? (current-input-port))
                                    (#%file-input-port? (current-input-port)))
                                maybe-next-line
                                (begin
                                  (set! *reader* (reader.new-reader))
                                  (error "missing closing paren - unexpected eof")))
                            ;; If the next line is not empty,
                            (begin
                              (reader.reader-push-string *reader* maybe-next-line)
                              (read-impl finisher)))))
                    next))]

             [else next-line])))]

    ;; The reader is not empty!
    [else
     (let ([next (reader.reader-read-one *reader*)])

       ; (displayln "GETTING HERE")
       ; (displayln next)

       (if (void? next)
           ;; TODO: Share this code with the above
           (begin
             (let ([maybe-next-line (read-line-from-port (current-input-port))])
               (if (eof-object? maybe-next-line)
                   ;; Stupid hack
                   (if (or (#%string-input-port? (current-input-port))
                           (#%file-input-port? (current-input-port)))
                       maybe-next-line
                       (begin
                         ;; TODO: drain the reader - consider a separate function for this
                         (set! *reader* (reader.new-reader))
                         (error "missing closing paren - unexpected eof")))
                   ;; If the next line is not empty,
                   (begin
                     (reader.reader-push-string *reader* maybe-next-line)
                     (read-impl finisher)))))

           next))]))
