(require-builtin #%private/steel/reader as reader.)
(require "#%private/steel/ports")

(provide read
         read-syntax-object)

(define (read . port)
  (reader.#%intern (reader.reader-read-one (if (null? port)
                                               (current-input-port)
                                               (car port)))))

(define (read-syntax-object . port)
  (reader.reader-read-one-syntax-object (if (null? port)
                                            (current-input-port)
                                            (car port))))
