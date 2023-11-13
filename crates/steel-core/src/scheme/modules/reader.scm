(require-builtin #%private/steel/reader as reader.)
(require "steel/result")
(require "#%private/steel/control")

(provide read)

(define *reader* (reader.new-reader))

(define (read)
  (define value (read-impl))
  (if (Ok? value) (Ok->value value) (raise-error (Err->value value))))

(define (read-impl)

  (cond
    [(reader.reader-empty? *reader*)
     (define next-line (read-line-from-port (current-input-port)))

     (cond
       [(string? next-line)
        (reader.reader-push-string *reader* next-line)
        (reader.reader-read-one *reader*)]

       [else
        =>
        next-line])]

    ;; The reader is not empty!
    [else
     =>
     (let ([next (reader.reader-read-one *reader*)])

       (map-ok next
               (lambda (obj)
                 (if (void? next)
                     (begin
                       (displayln "pushing another string")
                       (reader.reader-push-string *reader* (read-line-from-port (current-input-port)))
                       (read-impl))
                     next))))]))
