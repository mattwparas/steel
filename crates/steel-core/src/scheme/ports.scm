(require "#%private/steel/control")

;; Make an opaque port that matches the port interface
; (struct OpaquePort (is-input write-line-thunk) )

;; Try this out?
(define current-input-port (make-parameter (#%default-input-port)))
(define current-output-port (make-parameter (#%default-output-port)))

(define (custom-simple-display x)
  (write-string (current-output-port) x))

(define (newline)
  (write-char (current-output-port) #\newline))

(define (custom-simple-displayln x)
  (custom-simple-display x)
  (newline))
