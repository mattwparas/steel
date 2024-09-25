(require-builtin #%private/steel/reader as reader.)
(require "steel/result")
(require "#%private/steel/control")

(provide read
         read-syntax-object)

(define *reader* (reader.new-reader))

(define (read . port)
  (if (null? port)
      (read-impl reader.reader-read-one)
      (parameterize ([current-input-port (car port)])
        (read-impl reader.reader-read-one))))

(define (read-syntax-object . port)
  (if (null? port)
      (read-impl reader.reader-read-one-syntax-object)
      (parameterize ([current-input-port (car port)])
        (read-impl reader.reader-read-one-syntax-object))))

(define (read-impl finisher)
  (cond
    [(reader.reader-empty? *reader*)
     (define next-line (read-line-from-port (current-input-port)))
     (cond
       [(string? next-line)
        (reader.reader-push-string *reader* next-line)

        (let ([next (finisher *reader*)])
          (if (void? next)
              (begin
                (reader.reader-push-string *reader* (read-line-from-port (current-input-port)))
                (read-impl finisher))
              next))]

       [else next-line])]

    ;; The reader is not empty!
    [else
     =>
     (let ([next (reader.reader-read-one *reader*)])

       (if (void? next)
           (begin
             (reader.reader-push-string *reader* (read-line-from-port (current-input-port)))
             (read-impl finisher))
           next))]))
