(require-builtin #%private/steel/reader as reader.)
(require "#%private/steel/ports")

(provide read
         read-syntax-object)

;; The port holds whatever text has been buffered but not yet consumed, so there
;; is no reader state out here to get shared between ports.
(define (read . port)
  (reader.#%intern
   (reader.reader-read-one (if (null? port) (current-input-port) (car port)))))

(define (read-syntax-object . port)
  (reader.reader-read-one-syntax-object
   (if (null? port) (current-input-port) (car port))))
