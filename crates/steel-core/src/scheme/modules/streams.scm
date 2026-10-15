(provide stream-cons
         empty-stream
         stream-empty?
         stream-car
         #%stream-cdr
         #%stream?)

;; A lazy stream: its first value, and a function that makes the rest of it
;; when called. The empty stream has neither.
(struct Stream (first rest)
  #:printer (lambda (stream printer) (printer "#<stream>")))

(define empty-stream (Stream void #f))

(define (#%stream? value)
  (Stream? value))

;;@doc
;; A stream of `first`, then the stream `rest` returns when it is called.
;;
;; (stream-cons any/c (-> stream?)) -> stream?
(define (stream-cons first rest)
  (unless (procedure? rest)
    (error "stream-cons takes a function in the second position"))
  (Stream first rest))

;;@doc
;; Whether `stream` has no values.
;;
;; (stream-empty? stream?) -> bool?
(define (stream-empty? stream)
  (unless (Stream? stream)
    (error "stream-empty? takes a stream"))
  (not (Stream-rest stream)))

;;@doc
;; The first value of `stream`.
;;
;; (stream-car stream?) -> any/c
(define (stream-car stream)
  (unless (Stream? stream)
    (error "stream-car takes a stream"))
  (Stream-first stream))

;; The function that makes the rest of `stream`
(define (#%stream-cdr stream)
  (unless (Stream? stream)
    (error "stream-cdr takes a stream, found: " stream))
  (Stream-rest stream))
