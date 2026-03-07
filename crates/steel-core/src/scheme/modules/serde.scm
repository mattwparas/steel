(provide serialize-to-file
         deserialize-from-file
         round-trip)

(define (serialize-to-file value file-path)
  (define as-bytes (~> value serialize-value serialized->bytes))
  (call-with-output-file file-path (lambda (p) (write-bytes as-bytes p)) #:exists 'truncate))

(define (deserialize-from-file file)
  (call-with-input-file file
                        (lambda (p) (~> p read-port-to-bytes bytes->serialized deserialize-value))))

;;@doc
;; Round trip the value across the world
(define (round-trip value)
  (~> value serialize-value deserialize-value))
