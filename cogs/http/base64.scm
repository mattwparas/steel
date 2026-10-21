(provide base64-encode
         base64-decode)

(define *alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(define *values-by-char*
  (let loop ([at 0]
             [table (hash)])
    (if (>= at 64)
        table
        (loop (+ at 1) (hash-insert table (string-ref *alphabet* at) at)))))

(define (digit at)
  (string-ref *alphabet* at))

;;@doc
;; Encodes bytes, or the utf8 bytes of a string, as base64 with padding.
;;
;; ```scheme
;; (base64-encode "user:password") ;; => "dXNlcjpwYXNzd29yZA=="
;; ```
(define (base64-encode input)
  (define data
    (if (string? input)
        (string->bytes input)
        input))
  (define len (bytes-length data))
  (define out (open-output-string))
  (let loop ([at 0])
    (when (< at len)
      (let* ([first (bytes-ref data at)]
             [second (if (< (+ at 1) len)
                         (bytes-ref data (+ at 1))
                         0)]
             [third (if (< (+ at 2) len)
                        (bytes-ref data (+ at 2))
                        0)]
             [packed (+ (* first 65536) (* second 256) third)])
        (display (digit (quotient packed 262144)) out)
        (display (digit (modulo (quotient packed 4096) 64)) out)
        (display (if (< (+ at 1) len)
                     (digit (modulo (quotient packed 64) 64))
                     #\=)
                 out)
        (display (if (< (+ at 2) len)
                     (digit (modulo packed 64))
                     #\=)
                 out)
        (loop (+ at 3)))))
  (get-output-string out))

;;@doc
;; Decodes base64 into bytes. Padding and whitespace are optional, and anything
;; outside the alphabet is an error.
(define (base64-decode str)
  (define digits
    (filter (lambda (value) value)
            (map (lambda (char)
                   (cond
                     [(char=? char #\=) #f]
                     [(char-whitespace? char) #f]
                     [else
                      (or (hash-try-get *values-by-char* char)
                          (error "base64: not a base64 digit:" char))]))
                 (string->list str))))
  (define out (bytes))
  (let loop ([rest digits])
    (cond
      [(empty? rest) out]
      [(equal? (length rest) 1) (error "base64: the input ends in the middle of a byte")]
      [else
       (let* ([taken (min 4 (length rest))]
              [at (lambda (index)
                    (if (< index taken)
                        (list-ref rest index)
                        0))]
              [packed (+ (* (at 0) 262144) (* (at 1) 4096) (* (at 2) 64) (at 3))])
         (bytes-push! out (quotient packed 65536))
         (when (>= taken 3)
           (bytes-push! out (modulo (quotient packed 256) 256)))
         (when (>= taken 4)
           (bytes-push! out (modulo packed 256)))
         (loop (if (<= taken 4)
                   (list-tail rest taken)
                   '())))])))
